import os
from dataclasses import dataclass
from flask import g
from flask_login import LoginManager, UserMixin, login_user as _fl_login_user, logout_user as _fl_logout_user, current_user as _fl_current_user
from passlib.hash import bcrypt
from . import utils
import pandas as pd
import sqlalchemy

AUTH_ENABLED = bool(int(os.getenv("AUTH_ENABLED", "1")))

login_manager = LoginManager()
login_manager.login_view = "/login"  # where to send unauth'd accounts

# ---- User model for Flask-Login ----
@dataclass
class User(UserMixin):
    user_id: int
    username: str
    email: str

    @property
    def id(self):
        return str(self.user_id)
    
# ---- Lookups ----
def _row_to_user(row) -> User:
    return User(user_id=int(row["user_id"]), username=row["username"], email=row["email"])

def get_user_by_username(username: str) -> User | None:
    username = username.strip().lower()
    eng = utils.db.get_database_engine(connect_timeout=5)
    q = sqlalchemy.text("""
        SELECT user_id, username, email
        FROM gen_management.accounts
        WHERE username = :u
        LIMIT 1;
    """)
    df = pd.read_sql(q, eng, params={"u": username})
    return _row_to_user(df.iloc[0]) if not df.empty else None

def get_user_by_id(user_id: int) -> User | None:
    eng = utils.db.get_database_engine(connect_timeout=5)
    q = sqlalchemy.text("""
        SELECT user_id, username, email
        FROM gen_management.accounts
        WHERE user_id = :id
        LIMIT 1;
    """)
    df = pd.read_sql(q, eng, params={"id": user_id})
    return _row_to_user(df.iloc[0]) if not df.empty else None

# ---- Password helpers ----
def verify_password(pw_plain: str, pw_hash: str) -> bool:
    return bcrypt.verify(pw_plain, pw_hash)

def hash_password(pw_plain: str) -> str:
    return bcrypt.hash(pw_plain)

# ---- Create user ----
def create_user(username: str, email: str, password_plain: str) -> User:
    """
    Inserts into gen_management.accounts using your defined columns.
    """
    username = username.strip().lower()
    pw_hash = hash_password(password_plain)
    eng = utils.db.get_database_engine(connect_timeout=5)
    q = """
        INSERT INTO gen_management.accounts (username, email, password_hash, account_created, last_login, total_logins)
        VALUES (:u, :e, :ph, NOW(), NOW(), 1)
        RETURNING user_id, username, email;
    """
    with eng.begin() as conn:
        row = conn.execute(
            sqlalchemy.text(q),
            {"u": username, "e": email, "ph": pw_hash}
        ).mappings().first()
    return User(user_id=int(row["user_id"]), username=row["username"], email=row["email"])

# ---- Update password ----
def update_password(user_id: int, new_hash: str) -> None:
    eng = utils.db.get_database_engine(connect_timeout=5)
    with eng.begin() as conn:
        conn.execute(
            sqlalchemy.text(
                "UPDATE gen_management.accounts SET password_hash = :ph WHERE user_id = :uid"
            ),
            {"ph": new_hash, "uid": user_id}
        )

# ---- Login/Logout wrappers ----
def login_user(user: User) -> None:
    # update last_login/total_logins
    eng = utils.db.get_database_engine(connect_timeout=5)
    with eng.begin() as conn:
        conn.execute(
            sqlalchemy.text("""
                UPDATE gen_management.accounts
                SET last_login = NOW(), total_logins = COALESCE(total_logins,0)+1
                WHERE user_id = :uid
            """),
            {"uid": user.user_id}
        )
    _fl_login_user(user)

def logout_user() -> None:
    _fl_logout_user()

def current_user() -> User | None:
    cu = _fl_current_user
    if getattr(cu, "is_authenticated", False):
        return User(user_id=int(cu.user_id), username=cu.username, email=cu.email)
    return None

# ---- Flask-Login loader ----
@login_manager.user_loader
def load_user(user_id: str):
    try:
        return get_user_by_id(int(user_id))
    except Exception:
        return None
