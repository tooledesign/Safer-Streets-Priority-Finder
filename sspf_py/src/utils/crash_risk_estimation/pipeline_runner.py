# define an ordered list of your pipeline‐step callables
from dataclasses import dataclass
from .pipeline_steps.disaggregation import (
    assign_roadway_to_bg,
    compute_initial_fatal_estimates,
    compute_scaled_fatal_estimates,
    aggregate_segment_level_fatal_estimates,
    upload_fatal_estimates,
)
from .pipeline_steps.non_fatal_estimates import (
    compute_non_fatal_estimates,
    upload_all_severity_estimates,
)

from .pipeline_steps.bayesian_updating import (
    compute_crash_counts,
    update_crash_estimates_from_observations,
    upload_updated_crash_estimates,
)
from .pipeline_steps.data_prep import (
    get_study_data,
)
from .datastore import DataStore
from dataclasses import field
from ...global_vars import ts_print

@dataclass
class Pipeline:
    pre_steps: list = field(default_factory=list)
    steps: list = field(default_factory=list)
    post_steps: list = field(default_factory=list)

    def getsteps(self) -> list:
        """
        Returns the complete list of steps in the pipeline.
        """
        return self.pre_steps + self.steps + self.post_steps
    
@dataclass
class DefaultPipeline(Pipeline):
    """
    Default pipeline for crash risk estimation.
    """
    def __init__(self):
        super().__init__(
            pre_steps=[],
            steps=[
                assign_roadway_to_bg,
                compute_initial_fatal_estimates,
                compute_scaled_fatal_estimates,
                aggregate_segment_level_fatal_estimates,
                compute_non_fatal_estimates,
                compute_crash_counts,
                update_crash_estimates_from_observations,
            ],
            post_steps=[],
        )

# @dataclass
# class TestPipeline(DefaultPipeline):
#     """
#     Test pipeline for crash risk estimation.
#     This is used for testing purposes and may include additional steps or configurations.
#     """
#     def __init__(self):
#         super().__init__()
#         self.pre_steps = [
#             # get_test_data
#         ]
#         self.post_steps = [
#             upload_fatal_estimates,
#             upload_all_severity_estimates,
#             upload_updated_crash_estimates,
#         ]

@dataclass
class ProductionPipeline(DefaultPipeline):
    """
    Production pipeline for crash risk estimation.
    This is the default pipeline used in production.
    """
    def __init__(self):
        super().__init__()
        self.pre_steps = [
            get_study_data,
        ]
        self.post_steps = [
            upload_fatal_estimates,
            upload_all_severity_estimates,
            upload_updated_crash_estimates,
        ]


def run_pipeline(pipeline: Pipeline, datastore: DataStore) -> DataStore:
    """
    Run the crash risk estimation pipeline.
    
    Args:
        pipeline (Pipeline): The pipeline to run.
    
    Returns:
        DataStore: The final datastore after running the pipeline.
    """
    steps = pipeline.getsteps()
    for step in steps:
        ts_print(f"[Pipeline] Running step: {step.__name__}")
        datastore = step(datastore)
    ts_print("[Pipeline] Pipeline execution completed.")

    return datastore    
