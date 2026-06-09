import subprocess, shlex, sys, shutil, tempfile, uuid, os
from pathlib import Path
import time
import textwrap
import yaml 

SECTION_ORDER = [
    "show_intro",
    "show_data_attributes",
    "show_descriptive_stats",
    "show_analysis_params",
    "show_sliding_windows",
    "show_models",
]

AUTO_SECTION_START = "<!-- AUTO-SECTIONS:START -->"
AUTO_SECTION_END = "<!-- AUTO-SECTIONS:END -->"

SECTION_SNIPPETS = {
    "show_intro": textwrap.dedent(
        """
        ::: {.content-visible when-meta="params.show_intro"}
        {{< include sections/01_intro.qmd >}}
        :::
        """
    ),
    "show_data_attributes": textwrap.dedent(
        """
        ::: {.content-visible when-meta="params.show_data_attributes"}
        {{< include sections/02_data_attribute_assignment.qmd >}}
        :::
        """
    ),
    "show_descriptive_stats": textwrap.dedent(
        """
        ::: {.content-visible when-meta="params.show_descriptive_stats"}
        {{< include sections/03_descriptive_statistics.qmd >}}
        :::
        """
    ),
    "show_analysis_params": textwrap.dedent(
        """
        ::: {.content-visible when-meta="params.show_analysis_params"}
        {{< include sections/04_analysis_paras.qmd >}}
        :::
        """
    ),
    "show_sliding_windows": textwrap.dedent(
        """
        ::: {.content-visible when-meta="params.show_sliding_windows"}
        {{< include sections/05_sliding_windows.qmd >}}
        :::
        """
    ),
    "show_models": textwrap.dedent(
        """
        ::: {.content-visible when-meta="params.show_models"}
        {{< include sections/06_safer_streets_model.qmd >}}
        :::
        """
    ),
}


def _inject_sections(qmd_path: Path, yaml_params: dict) -> None:
    """Populate the AUTO-SECTIONS placeholder with enabled section includes."""

    if not qmd_path.exists():
        return

    text = qmd_path.read_text()
    sections = [
        SECTION_SNIPPETS[key].strip()
        for key in SECTION_ORDER
        if yaml_params.get(key, True)
    ]
    block = "\n\n".join(sections)
    replacement = AUTO_SECTION_START + "\n"
    if block:
        replacement += block + "\n"
    replacement += AUTO_SECTION_END
    if AUTO_SECTION_START in text and AUTO_SECTION_END in text:
        before, remainder = text.split(AUTO_SECTION_START, 1)
        _, after = remainder.split(AUTO_SECTION_END, 1)
        text = before + replacement + after
    else:
        if not text.endswith("\n"):
            text += "\n"
        text += "\n" + replacement + "\n"
    qmd_path.write_text(text)

def render_report(
    qmd_path="report.qmd",
    output_dir="output",
    output_name="safer-streets-report.pdf",
    yaml_params=None,
    py_params=None,
    quarto_bin="quarto",
    debug=False,
):
    """
    Render a Quarto `.qmd` report to PDF in an isolated build directory.

    The function prepares metadata and execution parameters, invokes
    Quarto, and moves the generated PDF (and optional TeX file) into a
    specified output directory. Each invocation uses a unique temporary
    build folder to avoid conflicts when multiple reports are rendered
    concurrently.

    Parameters
    ----------
    qmd_path : str or Path, optional
        Path to the Quarto `.qmd` source file. Relative paths are resolved
        relative to this script. Default is `"report.qmd"`.
    output_dir : str or Path, optional
        Destination directory for the final PDF (and optional TeX file).
        Created if it does not exist. Default is `"output"`.
    output_name : str, optional
        File name of the generated PDF. Default is `"safer-streets-report.pdf"`.
    yaml_params : dict, optional
        Document-level parameters mirrored into the Quarto metadata and
        used by this script when injecting section includes prior to
        rendering.
    py_params : dict, optional
        Execution parameters passed into the Jupyter/Papermill parameter
        cell. Keys must match variables tagged with `#| tags: [parameters]`
        in the report source.
    quarto_bin : str, optional
        Name or path to the Quarto executable. Default is `"quarto"`.
    debug : bool, optional
        If True, the intermediate `.tex` file is also moved into the output
        directory for inspection. Default is False.

    Returns
    -------
    pathlib.Path
        Absolute path to the generated PDF file.

    Raises
    ------
    FileNotFoundError
        If the input QMD file cannot be located.
    RuntimeError
        If the Quarto render fails or the expected output artifacts
        (PDF and, when requested, TeX) are not produced.
    """

    yaml_params = yaml_params or {}
    py_params = py_params or {}

    if "study_name" not in yaml_params:
        yaml_params["study_name"] = "Safer Streets Study"
    if "report_date" not in yaml_params:
        yaml_params["report_date"] = time.strftime("%Y-%m-%d")

    script_dir = Path(__file__).resolve().parent
    qmd = (script_dir / qmd_path) if not Path(qmd_path).is_absolute() else Path(qmd_path)
    qmd = qmd.resolve()
    if not qmd.exists():
        raise FileNotFoundError(f"Input QMD not found: {qmd}")

    target_path = (Path(output_dir) / output_name).resolve()
    target_path.parent.mkdir(parents=True, exist_ok=True)

    temp_root = Path(tempfile.mkdtemp(prefix="sspf-report-build-"))
    build_dir = temp_root / str(uuid.uuid4())
    build_dir.mkdir(parents=True, exist_ok=True)

    try:
        src_root = qmd.parent
        shutil.copytree(src_root, build_dir, dirs_exist_ok=True)
        qmd_in_build = build_dir / qmd.name
        _inject_sections(qmd_in_build, yaml_params)

        sspf_src_dir = script_dir.parent
        if sspf_src_dir.exists():
            shutil.copytree(sspf_src_dir, build_dir / "src", dirs_exist_ok=True)
            print(f"✔ Copied sspf_py source to {build_dir}")
        else:
            print(f"⚠️ sspf_py/src not found at {sspf_src_dir}")

        meta_yaml_path = build_dir / "metadata.yaml"
        exec_yaml_path = build_dir / "execute_params.yaml"
        meta_yaml_path.write_text(yaml.safe_dump({"params": yaml_params}, sort_keys=False))
        exec_yaml_path.write_text(yaml.safe_dump(py_params, sort_keys=False))

        out_name = target_path.name
        args = [
            quarto_bin,
            "render",
            str(qmd_in_build),
            "--to",
            "pdf",
            "--output",
            out_name,
            "--metadata-file",
            str(meta_yaml_path),
            "--execute-params",
            str(exec_yaml_path),
        ]

        print("Running:", " ".join(shlex.quote(a) for a in args))
        env = os.environ.copy()
        venv_bin = os.path.dirname(sys.executable)
        tinytex_bin = str(Path.home() / ".TinyTeX/bin/x86_64-linux")
        new_path = venv_bin + os.pathsep + env.get("PATH", "")
        if os.path.isdir(tinytex_bin):
            new_path = tinytex_bin + os.pathsep + new_path
        env["PATH"] = new_path

        # Per-render side-channel for traceback forwarding from inside
        # the ipykernel subprocess back to the dash log. See
        # report_utils/render_guard.py for the full rationale.
        guard_log_fd, guard_log_path = tempfile.mkstemp(prefix="report_generator_", suffix=".log")
        os.close(guard_log_fd)
        guard_log = Path(guard_log_path)
        env["REPORT_GENERATOR_LOG"] = str(guard_log)
        try:
            try:
                subprocess.run(args, check=True, cwd=str(build_dir), env=env)
            except subprocess.CalledProcessError as exc:
                raise RuntimeError(f"Quarto render failed with exit code {exc.returncode}.") from exc
        finally:
            if guard_log.exists() and guard_log.stat().st_size:
                print(f"--- render_guard tracebacks for {out_name} ---")
                print(guard_log.read_text())
                print(f"--- end render_guard tracebacks ---")
            guard_log.unlink(missing_ok=True)

        # Quarto writes the PDF to the temp build dir, so move it to the target path. 
        produced_pdf = build_dir / out_name
        if not produced_pdf.exists():
            raise RuntimeError("Render finished without an output file; check logs above.")
        if target_path.exists():
            target_path.unlink()
        shutil.move(str(produced_pdf), str(target_path))
        print(f"✔ PDF written to {target_path}")

        if debug:
            tex_path = build_dir / qmd.name.replace(".qmd", ".tex")
            if not tex_path.exists():
                raise RuntimeError("TeX file not found; check logs above.")
            debug_tex_path = target_path.parent / tex_path.name
            if debug_tex_path.exists():
                debug_tex_path.unlink()
            shutil.move(str(tex_path), str(debug_tex_path))
            print(f"✔ TeX written to {debug_tex_path}")

        return target_path
    finally:
        shutil.rmtree(temp_root, ignore_errors=True)
