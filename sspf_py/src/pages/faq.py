from dash import dcc, html, Input, Output, clientside_callback
import dash_bootstrap_components as dbc
from ..global_vars import CONTACT_EMAIL


FAQ_DATA = {
    "Basics": [
        {
            "question": "Who are the intended users of the Safer Streets Priority Finder (SSPF)?",
            "answer": (
                "The SSPF was designed for use by planners and engineers working for municipal "
                "government agencies or departments (city, county, regional, state, federal, "
                "etc.) who have a basic familiarity with GIS data and traffic safety and are engaged "
                "directly in project prioritization and implementation. Advocates and interested "
                "members of the public are also welcome to use it. For agencies that wish to "
                "share the outputs or results broadly with the public, we recommend exporting the "
                "results (e.g., reports, images, GIS layers) and hosting them on the agency's "
                "website. This tool is not intended to be used for interactive public engagement."
            ),
        },
        {
            "question": "Do I need to know how to use GIS if I want to use the SSPF?",
            "answer": (
                "This tool was designed assuming minimal familiarity with GIS. If you have "
                "access to datasets that are already properly formatted, or if you are using the "
                "default datasets, no additional GIS knowledge is needed to load data, run analyses, "
                "or visualize results within the tool. Users will need to be familiar enough "
                "with the data they are uploading to indicate which fields correspond to the "
                "information used by the tool. Some GIS familiarity is helpful for people who need to "
                "reformat their data, or who wish to export their results for further analysis."
            ),
        },
        {
            "question": "Do I need to be familiar with traffic safety or systemic safety analysis to use the SSPF?",
            "answer": (
                "Baseline familiarity with traffic safety or systemic safety analysis is "
                "helpful for context in understanding and applying the SSPF outputs. Please use "
                "caution when interpreting data outputs. There are limitations with the available "
                "outputs that have been documented throughout the SSPF and are important to review."
            ),
        },
        {
            "question": "How long does a typical study take to run?",
            "answer": [
                html.P([
                    "The time will depend on the number of factors - "
                    "number of concurrent users, size of study area, internet speeds, components of the analysis being run,"
                    "and complexity of the analysis and other applications running on the system. "
                    "Different components of the tool also take different amounts of time to run. "
                    "Most pages and processes take a few seconds to run, but some processes can take several minutes. "
                    "The largest study we tested with ~44,000 miles of roadway and ~200,000 crash records "
                    "had the following run times for each component (only those that took a minute or longer are listed): ",
                    html.Ul([
                        html.Li("Data upload (roads): ~1 minute."),
                        html.Li("Sliding Windows Analysis: ~1.5 minutes"),
                        html.Li("Safer Streets Model: ~5.5 minutes"),
                        html.Li("Report compilation: ~3 minutes"),
                        html.Li("GIS outputs export: ~1 minute"),
                        html.Li("Map visualizations: ~1 minute"),
                    ]),
                    "For some large areas we tested, the analysis took 5-10 minutes to run. "
                    "If your analysis is taking longer than 15 minutes to run, please reach out to the SSPF team at ",
                    html.A(CONTACT_EMAIL, href=f"mailto:{CONTACT_EMAIL}"),
                ]),
            ],
        },
        {
            "question": "What internet browsers are compatible with the SSPF?",
            "answer": "The SSPF can be used in Chrome, Safari, Firefox, and the newest version of Edge.",
        },
    ],
    "Data Inputs": [
        {
            "question": "What data do I need to use the SSPF?",
            "answer": (
                "The analysis needs a study area, roadways, and crash data. The analysis can be conducted either using 1) local data uploaded by the user "
                "or 2) nationally-available default data. If using user-uploaded data, study "
                "area, roadway network, and crash data is required. Users can choose a combination "
                "of local and default data for their study."
            ),
        },
        {
            "question": "How will my results vary based on whether I use default data versus my own local data?",
            "answer": (
                "We strongly encourage uploading local crash data when it is available with "
                "crash severity details, as the nationally-available FARS crash data used as the "
                "default is limited to fatal crashes. Because of the limited number of pedestrian "
                "and bicyclist fatalities in any one area, results based on default data will be "
                "sparse and should be used with caution. The Sliding Windows analysis using FARS "
                "data may look very sparse or unhelpful, as this analysis relies on severe and "
                "minor injury crashes to help understand connected corridors of higher crash risk. "
                "The model outputs are limited to just fatality based crashes if FARS data are used."
            ),
        },
        {
            "question": "Where can I find crash data with additional severity levels beyond fatal crashes?",
            "answer": [
                html.P([
                    "In the course of developing and testing this tool, a scan of state-level "
                    "publicly available datasets which may be suitable for use as an alternative to FARS "
                    "data was completed. The results of this scan are summarized ",
                    html.A("here,", href="./crash_data_sources", target="_blank", style={"color": "#007bff"}),
                    " as well as a few City-level open data portals identified during "
                    "the process. While many governmental agencies can readily access detailed crash "
                    "data from state DOTs, either through a web portal or by request, general "
                    "public/non-governmental access to spatial data necessary to utilize this tool varies "
                    "widely. Many states have developed open data portals where crash data may be "
                    "visualized and/or downloaded in spatial or tabular formats, while others only provide "
                    "summary statistics about recent crash histories in a jurisdiction. This scan does "
                    "not represent a comprehensive list of state-level data sources; users are "
                    "encouraged to seek additional resources not listed here for their state or local "
                    "jurisdiction. In addition, the availability of spatial data does not necessarily "
                    "imply that available datasets will include all attributes necessary for use with "
                    "this tool. For some datasets, extensive data processing and/or specialized "
                    "technical capabilities may be required to derive a shapefile suitable for use. This "
                    "table is included as a starting point for identifying local crash data.",
                ]),
            ],
        },
        {
            "question": "In what cases should I run a new study?",
            "answer": (
                "A new study would capture changes to any of the following: study area, time "
                "period (crash data), or data source (roadway or crash data). Additional studies "
                "can be used to run an analysis larger than one county, or when the county of "
                "interest is large and is processing slowly--in these cases splitting the study area "
                "into smaller sections will improve processing time. To do this, you will need to "
                "open your input datasets in GIS software and split them into separate study portions."
            ),
        },
        {
            "question": "How were the default crash costs developed, and when should I use default costs versus inputting custom costs?",
            "answer": [
                html.P([
                    "This tool uses crash costs provided by ",
                    html.A(
                        "FHWA's Crash Costs for Highway Safety Analysis (2024)",
                        href="https://highways.dot.gov/sites/fhwa.dot.gov/files/2025-10/CrashCostFactSheet_508_OCT2025.pdf",
                        target="_blank",
                        style={"color": "#0267FD"},
                    ),
                    "These costs are not localized for states--they do not reflect "
                    "state-specific injury to crash incidences used to develop crash unit costs from "
                    "person-injury unit costs, and will not reflect state-specific cost of living adjustment "
                    "factors. For localized crash costs, the user will need to input their own costs "
                    "(see below for information on how to find local crash costs). Users also have "
                    "the option to customize the discount rate which is applied to reflect today's "
                    "value of costs projected over a time horizon of five years.",
                ])
            ],
        },
        {
            "question": "Where can I find local crash costs for my state?",
            "answer": [
                html.P([
                    "Typically, this is something published by state DOTs. You may use ",
                    html.A(
                        "FHWA's Crash Costs for Highway Safety Analysis Tool",
                        href="https://highways.dot.gov/safety/hsip/crash-costs-highway-safety-analysis-tool",
                        target="_blank",
                        style={"color": "#007bff"},
                    ),
                    " to calculate crash costs more tailored to your study area.",
                ]),
            ],
        },
        {
            "question": "For users uploading their own data",
            "answer": None,
            "items": [
                {
                    "question": "What geography should my study area encompass?",
                    "answer": (
                        "Counties are currently the largest available jurisdiction the SSPF is "
                        "intended to accommodate. If you wish to study a larger area, you can separately analyze "
                        "several counties and compile the results post-hoc in GIS. If your county is "
                        "large or contains a lot of crashes or roadway segments, or if the tool is running "
                        "slowly, you may need to split your study area into pieces. Note that the limiting "
                        "factor is the number of features (e.g., number of crashes, number of road "
                        "segments) and not absolute size of the geographic area."
                    ),
                },
                {
                    "question": "What is the largest file size I can upload?",
                    "answer": (
                        "For roads data, the maximum allowable length of roadway miles that can be "
                        "uploaded is 45,000 miles. For crash data, the maximum allowable number of crashes "
                        "is 250,000. If your data exceeds these limits, you will need to clip the data "
                        "prior to uploading it. Shapefiles uploaded cannot exceed 20 MB. "
                        "If necessary, you might consider dropping any unnessary fields from your data to reduce file size."
                    ),
                },
                {
                    "question": "How are dual carriageways handled?",
                    "answer": (
                        "Many agencies digitize their divided roadways as dual centerlines or dual "
                        "carriageways (i.e., two lines). In this case, crashes may be attached to one side "
                        "of the roadway or another. This may reduce the density of crashes per mile (i.e., "
                        "underestimate risk) on these portions of the network, since the mileage for "
                        "this type of road is represented twice. This may result in crash risk being "
                        "underestimated on divided roads in both the Sliding Windows analysis and the Safer "
                        "Streets Model. Advanced GIS users may wish to download and transform the analysis "
                        "outputs on dual carriageways to correct for this issue, though doing so is at your "
                        "own risk and the results have not been validated using this approach. For two "
                        "matched dual carriageway segments (i.e., segments representing either "
                        "direction of the same stretch of roadway), you can sum the output from each side, and "
                        "use the sum as the corrected output for both segments. Users should be wary when "
                        "interpreting model results for streets with dual centerlines."
                    ),
                },
            ],
        },
        {
            "question": "Assigning data inputs to information used in the tool",
            "answer": None,
            "items": [
                {
                    "question": "How do I assign my roadway data's functional classification to the options available if there isn't a one to one match?",
                    "answer": [
                        html.P([
                            "If a one to one match is not available, choose the closest option that "
                            "corresponds with your data's functional classification. The functional class is used "
                            "to inform the relative crash risk based on national data."
                            "FHWA definitions of functional classifications can be found ",
                            html.A(
                                "here.",
                                href="https://www.fhwa.dot.gov/policyinformation/hpms/hfcccp.cfm",
                                target="_blank",
                                style={"color": "#007bff"},
                            ),
                            " Note that the SSPF uses the term ",
                            html.I("Major Arterial"),
                            " instead of the FHWA's term ",
                            html.I("Other Principal Arterial. "),
                        ]),
                    ],
                },
                {
                    "question": "How are freeways/roadways with restricted access handled?",
                    "answer": [
                        html.P([
                            "Freeways and limited access roadways should be categorized under the 'Expressway' functional class. "
                            "While some states allow bicycle and pedestrian travel on some freeways, this is not consistent across the country. "
                            "In cases where freeways are not accessible to pedestrians and bicyclists, "
                            "it is likely that there will be very few crashes involving these modes on these roadways. "
                            "SSPF will still assign a crash risk score to these roadways using average crash rates for freeways, "
                            " calculated based on state-level averages which in some cases are imputed based on regional averages to fill missing data. "
                            "If your study is focused on bicycle and pedestrian modes and you have freeways in your dataset that are not accessible to these modes, "
                            "you may wish to omit these from your dataset before uploading to avoid skewing the results."
                        ]),
                    ],
                },
                {
                    "question": "How are multi-use paths handled?",
                    "answer": [
                        html.P([
                            "You can remove them from your dataset before uploading or group them as a "
                            "separate functional class, which can then be assigned to the ",
                            html.I("omit"),
                            " category.",
                        ]),
                    ],
                },
                {
                    "question": "What crash severity scale is accepted?",
                    "answer": [
                        html.P(
                            "The SSPF accepts crash severity according to the KABCO scale, which is used "
                            "in most police crash reports. The SSPF does not currently accommodate other crash "
                            "severity categories (such as injury/non-injury or MAIS)."
                        ),
                        html.P([
                            "MMUCC 6th Edition provides definitions for each KABCO level and is available ",
                            html.A(
                                "here.",
                                href="https://www.nhtsa.gov/traffic-records/model-minimum-uniform-crash-criteria",
                                target="_blank",
                                style={"color": "#007bff"},
                            ),
                        ]),
                        html.P(
                            "Note that excluding 'O' or 'PDO' crashes, especially motor vehicle PDO "
                            "crashes, will not meaningfully impact the model results. If you have a large crash "
                            "dataset with a lot of motor vehicle PDO crashes, consider dropping these from the "
                            "dataset before uploading to make the tool run faster."
                        ),
                    ],
                },
            ],
        },
    ],
    "Tool Analysis Outputs": [
        {
            "question": "What are the tool's primary outputs, and how do they differ?",
            "answer": [
                html.P(
                    "There are two discrete outputs from the SSPF - a Sliding Windows Analysis and "
                    "a Safer Streets Model. The Sliding Windows Analysis output reflects historical crashes, "
                    "while the Safer Streets Model output reflects estimated crash risk by combining "
                    "block group level fatal crash estimates with roadway functional class data and adjusting "
                    "this with observed crash data using an Empirical Bayes approach."
                ),
            ],
        },
        {
            "question": "How do the analyses available in the SSPF fit into broader Vision Zero principles?",
            "answer": (
                "Vision Zero is a movement and strategy based in the recognition that traffic "
                "deaths are unacceptable and preventable, which seeks to eliminate traffic deaths "
                "and severe injuries for all roadway users. Cities throughout the United States "
                "have made Vision Zero commitments. Elemental to Vision Zero is a data-driven, "
                "proactive, and systems-based approach to safety. The SSPF offers communities a "
                "starting point for safety analyses to provide an understanding of pedestrian and "
                "bicycle crash risk along their roadway network which can be used to prioritize "
                "safety investments. As part of Vision Zero, many communities create a High Injury "
                "Network (HIN). The outputs from this tool, both from the sliding windows analysis "
                "and the Safer Streets model, can be used as inputs to creating a HIN."
            ),
        },
        {
            "question": "How do the analyses available in the SSPF fit into the range of data analysis techniques commonly used to address traffic safety issues?",
            "answer": [
                html.P(
                    "A variety of data analysis techniques are commonly used by agencies to "
                    "address traffic safety issues. These include hot spot analyses, Sliding Windows "
                    "analyses/High Injury Networks, and systemic safety analyses. Each of these analyses, "
                    "along with the SSPF's related capabilities, are discussed below."
                ),
                html.P(
                    "Although hot spot analysis is relatively simple, it may miss clusters of "
                    "crashes along a corridor, is limited in its effectiveness for bicyclist and "
                    "pedestrian crashes, and doesn't account for the fact that hotspots move around a network "
                    "over time. Beyond visualizing crash data, the SSPF is not intended to provide "
                    "hot spot analysis capabilities."
                ),
                html.P(
                    "The SSPF provides a Sliding Windows analysis, which can be used to develop a "
                    "High Injury Network. Sliding Windows analyses addresses some of the shortcomings "
                    "of hot spot analyses by highlighting patterns of safety problems at the "
                    "corridor level, which can then be used to inform prioritization of safety treatments. "
                    "Typically, Sliding Windows analyses have a higher barrier to entry due to the "
                    "more complex GIS and coding skills needed to develop them. This tool lowers the "
                    "barrier to entry by automating this process. High Injury Networks are based on "
                    "sliding windows, but identify a subset of streets with the highest density of severe "
                    "crashes, allowing jurisdictions to prioritize investment and focus on the most "
                    "critical safety issues."
                ),
                html.P(
                    "Systemic safety analysis involves reviewing the characteristics of locations "
                    "where crashes occurred to understand the risk factors present at locations where "
                    "crashes have happened. Once risk factors have been identified, the network can "
                    "be screened for risk factors to understand where crashes are more likely to "
                    "occur in the future, whether or not there have been historical crashes in these "
                    "locations. Safety Performance Functions (SPFs), which are a key systemic safety "
                    "analysis method, require significant data and GIS/coding expertise to develop. The "
                    "Safer Streets Model is not as robust as a safety performance function and should "
                    "not be seen as a replacement for bicyclist- and pedestrian-specific safety "
                    "performance functions. It does not link results to specific roadway configurations on "
                    "the network. However, by using external data from national functional class "
                    "fatality rates and the Block Group Fatalities Model, the Safer Streets "
                    "Model adds a proactive element to the analysis above and beyond simple Sliding "
                    "Windows analysis. It is a useful entrypoint to systemic analysis for those with "
                    "limited GIS and coding skills or those without extensive roadway data."
                ),
            ],
        },
        {
            "question": "Safer Streets Model",
            "answer": None,
            "items": [
                {
                    "question": "How does the Safer Streets Model forward our understanding of safety across different modes?",
                    "answer": (
                        "While the Sliding Window analysis output network forms the basis of a typical "
                        "High Injury Network, the Safer Streets Model goes further than a typical High "
                        "Injury Network by factoring what else we know about risk from the Block Group Fatality "
                        "model and functional class crash rate averages in addition to historical crashes. "
                        "Crashes are statistically rare in many locations, especially for pedestrians and bicyclists. "
                        "In places with low crash volumes for a particular mode, you're unlikely to see that mode's crashes recorded. "
                        "The absence of crashes does not imply an absence of risk. The modeling framework provides a way to understand risk "
                        "across all modes even where there isn't sufficient crash history, but with a lower technical barrier to entry than systemic analysis "
                        "and network screening. The model incorporates crash costs by mode to enable outputs to be linked to the planning process."
                    ),
                },
                {
                    "question": "How was the model validated?",
                    "answer": [
                        html.P(
                            "The model was validated by testing how well it predicts future crashes - crashes "
                            "from a different time period than the ones used as input - and comparing it against "
                            "ranking road segments by crash history alone. Validation was carried out across three "
                            "study areas spanning different contexts: the City of New Orleans, LA (urban), the City "
                            "of Albany, OR (suburban), and Knox County, OH (rural)."
                        ),
                        html.P(
                            "For each area, a 10-year crash dataset was split into two halves. The first 5 years "
                            "stand in for the crash history an agency would have today, and the last 5 years (the "
                            "holdout) represent the future crash pattern the analysis is trying to anticipate. "
                            "Validation then examined what share of future severe crashes fell on the top-scoring "
                            "road segments - in other words, how often future crashes happened on roads the model "
                            "flagged as higher risk versus lower risk."
                        ),
                        html.P(
                            "In all three study areas, the model matched or out-performed crash history alone, with "
                            "the largest gains where crash history is least reliable: the rural study area and "
                            "pedestrian and bicyclist crashes. In the rural case, the share of future pedestrian and "
                            "bicycle crashes captured within the top 25% of segments rose from 34% to 68% and from "
                            "53% to 90%, respectively, over crash history alone. Where observed crashes are dense, "
                            "such as urban motor-vehicle crashes, crash history already ranks well, and the model "
                            "is similar and does not degrade performance. In short, the model adds the most "
                            "where crash data is sparse and steps back where the historical record is already "
                            "informative."
                        ),
                    ],
                },
                {
                    "question": "How do I interpret the cost estimates provided by the Safer Streets Model?",
                    "answer": [
                        html.P(
                            "The crash cost for a given mode is calculated as the sum of the crash costs across all severity levels for that mode. "
                            "The crash costs for a given mode and severity are calculated by multiplying the crash costs for that severity "
                            "(provided in analysis settings) by the crash estimates for that severity from the model. "
                            "The crash costs represents what is expected to be lost in terms of life and economic value "
                            "from crashes on a given road segment over a five year period. "
                        ),
                    ],
                },
                {
                    "question": "How should I interpret models that utilize only FARS data?",
                    "answer": (
                        "FARS data only has fatal crash information. Model outputs are only calculated for fatal mode for these cases. "
                        "This also applies to local crash data if it only has fatal crash information. "
                        "If you wish to estimate non-fatal crash risk, you may use the crash severity ratio tables "
                        "which is one of the static inputs to the tool. See tool codebase or documentation for more details."
                    ),
                },
                {
                    "question": "How well does the model work in rural areas?",
                    "answer": (
                        "Rural areas tend to have low number of observed crashes which could "
                        "make the model estimated crashes vary heavily from those of observed. "
                        "Due to the spareseness of crashes, the model outputs in rural areas "
                        "are more heavily informed by the priors from the Block Group Fatalities Model and national functional class fatality rates. "
                        "These outputs should be used with caution in rural areas with low observed crashes."
                    ),
                },
                {
                    "question": "How should I interpret model fit charts within the dashboard?",
                    "answer": [
                        html.P(
                            "The model fit chart in the Dashboard tab shows the distribution of observed "
                            "and estimated crashes by mode and severity. It is likely that "
                            "the total model estimated crashes are different from the observed crashes due to "
                            "the different ways the tool tallies observed crashes and model crashes. "
                            "The chart is meant to provide a general sense of how the model estimated crashes compare to observed crashes "
                            "in terms of their distribution across modes and severity levels. "
                            "If the model fit chart shows a big difference between observed and model estimated crashes, "
                            "please use caution when interpreting the model outputs.",
                        ),
                    ],
                },
                {
                    "question": "Why does my model indicate crash risk even in areas where there were no historic crashes?",
                    "answer": (
                        "When there are no observed crashes of a given type, the model still assigns "
                        "the crash risk based on the street's functional classification and the census "
                        "block groups that it is part of. Some segments which are part of a block group with high "
                        "fatality model values may see a higher crash risk. The absence of crashes does not imply an absence of risk - "
                        "demonstrating the risk that exists on roads that have not had crashes in the "
                        "past is central to a systemic safety approach."
                    ),
                },
                {
                    "question": "How do I use the model outputs?",
                    "answer": [
                        html.P(
                            "The two main types of outputs from the model are the model estimated crash total by mode/severity and "
                            "crash costs by mode. The unit of analysis is the short windows (default 0.1 mile - adjustable in analysis settings). "
                            "Since these unit of analysis segments are short and generally tend to be relatively uniform in length, "
                            "the tool outputs are not normalized by length and represent an absolute crash risk and cost for each segment and not their rates. "
                            "The map visualization page within the tool shows these individual unit of analysis segments. "
                            "You may wish to aggregate these outputs to the corridor or neighborhood level for prioritization and investment decisions. "
                            "This can be done by downloading the model outputs and post-processing them in GIS software. "
                        ),
                    ],
                },
            ],
        },
    ],
}


clientside_callback(
    """
    function(hash) {
        if (!hash) {
            return "";
        }

        const targetId = hash.replace('#', '');
        const scrollToTarget = () => {
            const target = document.getElementById(targetId);
            if (target) {
                target.scrollIntoView({ behavior: 'smooth', block: 'start' });
            }
        };

        // Render timing can vary in Dash pages; try immediately and once after paint.
        scrollToTarget();
        window.requestAnimationFrame(() => window.setTimeout(scrollToTarget, 0));

        return "";
    }
    """,
    Output("faq-hash-scroll-trigger", "children"),
    Input("faq-location", "hash"),
)


def _render_answer(answer):
    if answer is None:
        return []
    if isinstance(answer, str):
        return [html.P(answer)]
    return [el if not isinstance(el, str) else html.P(el) for el in answer]


def _render_review_box(review_text):
    if not review_text:
        return []

    return [
        html.Div(
            [
                html.Strong("Attention", style={"color": "#F57F17"}),
                html.P(review_text, className="mb-0 mt-1"),
            ],
            style={
                "backgroundColor": "#FFF8E1",
                "border": "1px solid #FBC02D",
                "borderLeft": "6px solid #F57F17",
                "borderRadius": "6px",
                "padding": "0.75rem 1rem",
                "marginBottom": "0.75rem",
            },
        )
    ]


def _build_faq():
    _LINK = {"color": "#007bff"}
    toc = []
    body = []
    anchor = 0

    for s_idx, (section, questions) in enumerate(FAQ_DATA.items()):
        s_num = s_idx + 1
        anchor += 1
        s_label = f"{s_num}. {section}"
        toc.extend([html.A(s_label, href=f"#faq-{anchor}", style=_LINK), html.Br()])
        body.append(html.H2(s_label, id=f"faq-{anchor}"))

        for q_idx, q in enumerate(questions):
            q_num = q_idx + 1
            anchor += 1
            q_label = f"{s_num}.{q_num} {q['question']}"

            if "items" in q:
                toc.extend([html.Span(" "), html.A(q_label, href=f"#faq-{anchor}", style=_LINK), html.Br()])
                body.append(html.H3(q_label, id=f"faq-{anchor}"))
                body.extend(_render_review_box(q.get("review_box")))
                for sub_idx, sub in enumerate(q["items"]):
                    sub_num = sub_idx + 1
                    anchor += 1
                    sub_label = f"{s_num}.{q_num}.{sub_num} {sub['question']}"
                    toc.extend([html.Span("  "), html.A(sub_label, href=f"#faq-{anchor}", style=_LINK), html.Br()])
                    body.append(html.H4(sub_label, id=f"faq-{anchor}"))
                    body.extend(_render_review_box(sub.get("review_box")))
                    body.extend(_render_answer(sub["answer"]))
            else:
                toc.extend([html.Span(" "), html.A(q_label, href=f"#faq-{anchor}", style=_LINK), html.Br()])
                body.append(html.H3(q_label, id=f"faq-{anchor}"))
                body.extend(_render_review_box(q.get("review_box")))
                body.extend(_render_answer(q["answer"]))

    return toc, body


def layout():
    toc, body = _build_faq()
    return dbc.Container(
        [
            dcc.Location(id="faq-location", refresh=False),
            html.Div(id="faq-hash-scroll-trigger", style={"display": "none"}),
            html.H3("Frequently Asked Questions", id="faq-toc", className="mb-3"),
            html.P([
                "If you have questions about the tool that are not answered here or any accompanying materials, please reach out to the SSPF team at ",
                html.A(CONTACT_EMAIL, href=f"mailto:{CONTACT_EMAIL}")
            ]),
            html.P(toc, className="mb-4"),
            *body,
        ],
        fluid=True,
        className="pb-4",
    )
