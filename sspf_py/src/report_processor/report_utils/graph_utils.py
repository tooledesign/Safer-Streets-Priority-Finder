"""Helper functions for embedding Plotly visuals inside Quarto reports."""

from typing import Any

from IPython.display import Image, Markdown, display
import plotly.io as pio


def render_plotly_figure(
    fig: Any,
    *,
    width: int = 850,
    height: int = 500,
    scale: int = 2,
    alt_text: str = "No chart available.",
) -> None:
    """Render a Plotly figure as a PNG to keep PDF output deterministic."""
    if fig is None:
        display(Markdown(f"_{alt_text}_"))
        return
    try:
        img_bytes = pio.to_image(
            fig,
            format="png",
            width=width,
            height=height,
            scale=scale,
        )
    except Exception as exc:  # pragma: no cover - runtime helper
        display(Markdown(f"_Unable to render figure: {exc}_"))
        return
    display(Image(data=img_bytes))
