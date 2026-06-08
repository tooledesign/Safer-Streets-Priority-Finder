from IPython.display import Markdown, display
import pandas as pd
import re


_LATEX_SPECIAL_CHARS = {
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
    "~": r"\textasciitilde{}",
    "^": r"\textasciicircum{}",
    "\\": r"\textbackslash{}",
}


def _coerce_float(value):
    try:
        return float(value)
    except (TypeError, ValueError):
        return 0.0


def format_count(value):
    return int(round(_coerce_float(value)))


def format_percent(value):
    return f"{_coerce_float(value) * 100:.1f}%"


def format_miles(value):
    return f"{_coerce_float(value):,.1f}"


def format_currency(value):
    return f"${_coerce_float(value):,.0f}"


def _normalize_to_string(value):
    if value is None:
        return ""
    try:
        if value != value:
            return ""
    except Exception:
        pass
    return str(value)


def _escape_latex(value):
    if not value:
        return ""
    return "".join(_LATEX_SPECIAL_CHARS.get(char, char) for char in value)


def _format_cell(value):
    normalized = _normalize_to_string(value).replace("\\n", "\n")
    parts = normalized.split("\n")
    escaped_parts = [_escape_latex(part) for part in parts]
    return escaped_parts, len(parts) > 1


def _assemble_cell(parts, force_makecell):
    if not parts:
        parts = [""]
    if force_makecell or len(parts) > 1:
        content = r" \\ ".join(parts)
        return rf"\makecell[l]{{{content}}}"
    return parts[0]


def _build_column_spec(column_count, use_multiline_cells):
    if not use_multiline_cells:
        return "l" * column_count
    width = 0.95 / max(1, column_count)
    return "".join(
        [
            rf">{{\raggedright\arraybackslash}}m{{{width:.4f}\linewidth}}"
            for _ in range(column_count)
        ]
    )


def _build_latex_table(headers, rows, contains_multiline):
    column_count = len(headers)
    if column_count == 0:
        return ""
    column_spec = _build_column_spec(column_count, contains_multiline)
    row_separator = None
    if contains_multiline:
        row_separator = rf"\arrayrulecolor[gray]{{0.8}}\cmidrule(lr){{1-{column_count}}}\arrayrulecolor{{black}}"
    lines = [
        rf"\begin{{longtable}}{{{column_spec}}}",
        r"\toprule",
        " & ".join(headers) + r" \\",
        r"\midrule",
        r"\endfirsthead",
        r"\toprule",
        " & ".join(headers) + r" \\",
        r"\midrule",
        r"\endhead",
        r"\midrule",
        rf"\multicolumn{{{column_count}}}{{r}}{{\textit{{Continued on next page}}}} \\",
        r"\midrule",
        r"\endfoot",
        r"\bottomrule",
        r"\endlastfoot",
    ]
    for idx, row in enumerate(rows):
        lines.append(" & ".join(row) + r" \\")
        if row_separator and idx < len(rows) - 1:
            lines.append(row_separator)
    lines.append(r"\end{longtable}")
    return "\n".join(lines)


def render_latex_table(df, formatters=None):
    """
    For rendering LaTex tables from DataFrames.
    Block output must be set to asis.
    Useful for tables with multi-line cells.
    Drawback: will not auto scale to fit page width.
    
    :param df: Description
    :param formatters: Description
    """
    if df.empty:
        print("No data available for this study.")
        return
    table = df.copy()
    
    # allow no formatter
    formatters = formatters or {}

    for column, formatter in formatters.items():
        if column in table.columns:
            table[column] = table[column].apply(formatter)
    header_cells = []
    has_multiline = False
    for column in table.columns:
        parts, multiline = _format_cell(column)
        header_cells.append((parts, multiline))
        has_multiline = has_multiline or multiline

    row_cells = []
    for _, row in table.iterrows():
        formatted_row = []
        for value in row:
            parts, multiline = _format_cell(value)
            formatted_row.append((parts, multiline))
            has_multiline = has_multiline or multiline
        row_cells.append(formatted_row)

    headers = [_assemble_cell(parts, multiline) for parts, multiline in header_cells]
    latex_rows = [
        [_assemble_cell(parts, multiline) for parts, multiline in row]
        for row in row_cells
    ]

    latex_table = _build_latex_table(headers, latex_rows, has_multiline)
    print(latex_table)

def render_markdown_table(df: pd.DataFrame, smaller: bool = False):
    if df.empty:
        print("No data available for this study.")
        return
    md_table = df.to_markdown(index=False)
    if smaller:
        # wrap in LaTeX to reduce font size
        wrapped = (
            r"\begingroup\footnotesize"
            "\n\n" + md_table + "\n\n" + r"\endgroup"
        )
        display(Markdown(wrapped))
    else:
        display(Markdown(md_table)) 


_COUNT_PERCENT_PATTERN = re.compile(
    r"^\s*(?P<count>[0-9,]+)(?:\s*\((?P<pct>[-0-9.]+)%\))?\s*$"
)


def _split_count_pct(value):
    if value is None:
        return "", ""
    if isinstance(value, (int, float)):
        return str(int(value)), ""
    text = str(value).strip()
    match = _COUNT_PERCENT_PATTERN.match(text)
    if not match:
        return text, ""
    count_str = (match.group("count") or "").replace(",", "")
    pct_str = match.group("pct") or ""
    try:
        count_val = int(float(count_str))
        count_formatted = f"{count_val:,}"
    except ValueError:
        count_formatted = match.group("count") or text
    if pct_str:
        try:
            pct_val = float(pct_str)
            pct_formatted = f"{pct_val:.1f}%"
        except ValueError:
            pct_formatted = f"{pct_str}%"
    else:
        pct_formatted = ""
    return count_formatted, pct_formatted


def split_count_percent_tables(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    if df.empty:
        return df.copy(), df.copy()
    first_col = df.columns[0]
    counts = df[[first_col]].copy()
    percents = df[[first_col]].copy()
    for col in df.columns[1:]:
        counts[col] = df[col].apply(lambda v: _split_count_pct(v)[0])
        percents[col] = df[col].apply(lambda v: _split_count_pct(v)[1])
    return counts, percents
