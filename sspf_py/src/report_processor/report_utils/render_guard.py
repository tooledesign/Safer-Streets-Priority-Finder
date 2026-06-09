"""Context manager for graceful error handling in Quarto report sections.

Cross-process traceback forwarding
----------------------------------
Report sections execute inside an ``ipykernel`` subprocess spawned by
``quarto render``, which itself runs as a subprocess of the dash app::

    dash app
      └── quarto (subprocess.run, inherits dash stdout/stderr)
            └── ipykernel
                  └── this code

When :func:`render_guard` catches an exception we want the traceback to
end up in the same log as the dash app (journald on AWS, terminal in
dev) for debugging, while the rendered PDF only shows a friendly
fallback message.

The usual channels do not work here:

* ``traceback.print_exc()`` writes to ``sys.stderr``, which ipykernel
  has replaced with a ZMQ-backed stream. Output goes into the cell's
  output bundle (and into the PDF if not suppressed), never reaching
  quarto's terminal.
* Writing to ``sys.__stderr__`` / fd 2 hits the kernel subprocess's
  real stderr, which jupyter_client pipes internally rather than
  forwarding to quarto's parent process.
* Re-raising defeats the purpose of ``render_guard`` -- the section
  fails the whole render under ``error: false``.

Instead we use a file as a side channel:

1. ``report_processor.render_report`` creates a per-render tempfile and
   exports its path in the ``REPORT_GENERATOR_LOG`` env var on the
   quarto subprocess.
2. ``render_guard`` reads the env var and appends the traceback to that
   file when it catches an exception.
3. After ``subprocess.run`` returns, ``render_report`` reads the file
   and prints its contents to its own stdout, which is inherited by
   the dash app and so lands in the dash log.

The env var is an internal contract between these two modules; it is
not user-configurable. A more "standard" alternative on AWS would be
``systemd.journal.send`` from the cell side, but that depends on the
``python-systemd`` package and a journald host, so file IPC is
preferred for portability across dev/Docker/AWS.
"""

import os
import traceback
from contextlib import contextmanager

from IPython.display import Markdown, display


@contextmanager
def render_guard(fallback_msg="This section could not be rendered."):
    """Suppress exceptions and display a fallback message in the report output.

    If the ``REPORT_GENERATOR_LOG`` environment variable is set (by
    ``report_processor.render_report``), the traceback is appended to
    that file so the dash app can surface it in its own log after the
    render subprocess finishes. See the module docstring for the full
    rationale.

    Usage::

        with render_guard("Sliding window results are not available."):
            results = get_sliding_window_results(study_id)
            ...
    """
    try:
        yield
    except Exception:
        log_path = os.environ.get("REPORT_GENERATOR_LOG")
        if log_path:
            try:
                with open(log_path, "a") as f:
                    f.write("=" * 60 + "\n")
                    traceback.print_exc(file=f)
            except Exception:
                pass
        display(Markdown(f"_{fallback_msg}_"))
