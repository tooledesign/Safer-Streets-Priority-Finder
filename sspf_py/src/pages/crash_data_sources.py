from dash import html, dcc
import dash_bootstrap_components as dbc
import pandas as pd
from ..global_vars import CONTACT_EMAIL


# Data extracted from public datasets index.html
CRASH_DATA_SOURCES = [
    {"state": "Alabama", "portal": "AL Safety Portal", "url": "https://safety.aladata.com/", "publicly_available": "No", "notes": "Dashboard and summary data only; spatial data available through CARE with signed data use agreement"},
    {"state": "Alaska", "portal": "Alaska Highway Safety Office - Motor Vehicle Crash Data", "url": "https://dot.alaska.gov/stwdplng/hwysafety/crash.shtml", "publicly_available": "No", "notes": "PDF reports only"},
    {"state": "Arizona", "portal": "ADOT Arizona Motor Vehicle Crash Facts", "url": "https://azdot.gov/motor-vehicles/statistics/arizona-motor-vehicle-crash-facts", "publicly_available": "No", "notes": "Detailed data available through public records request"},
    {"state": "Arkansas", "portal": "ACAT (Arkansas Crash Analytics Tool)", "url": "https://ardot.maps.arcgis.com/apps/MapSeries/index.html?appid=7976060331fb4930933bf560f8a9c91b", "publicly_available": "Limited", "notes": "Fatal and Serious Injury crashes mapped - downloadability unclear"},
    {"state": "California", "portal": "SWITRS (Statewide Integrated Traffic Records System)", "url": "https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system", "publicly_available": "Yes", "notes": "Comprehensive relational database files available with registration"},
    {"state": "Colorado", "portal": "Colorado DOT Crash Data", "url": "https://www.codot.gov/safety/traffic-safety/crash-data-management/crash-data", "publicly_available": "Yes", "notes": "Tabular data with Lat/Lon available for download", "last_verified": "May 2026"},
    {"state": "Denver", "portal": "Denver Open Data Catalog", "url": "https://opendata-geospatialdenver.hub.arcgis.com/datasets/db00bd99ea534d8987e0913a191ebe19_325/explore", "publicly_available": "Yes", "notes": "Tabular or GIS downloads available", "last_verified": "May 2026"},
    {"state": "Connecticut", "portal": "Connecticut Crash Data Repository", "url": "https://www.ctcrash.uconn.edu/", "publicly_available": "Yes", "notes": "Detailed data available through query tool with registration", "last_verified": "May 2026"},
    {"state": "Delaware", "portal": "DelDOT Dashboard", "url": "https://deldot.gov/dashboard/index.shtml?dc=safety", "publicly_available": "Limited", "notes": "Crash data available by request with notarized data release agreement; request Portal under development as of 2019"},
    {"state": "Florida", "portal": "FDOT State Safety Office", "url": "https://www.fdot.gov/safety/safetyengineering/crash-data.shtm", "publicly_available": "Yes", "notes": "Shapefiles and tabular data available via Open Data Hub"},
    {"state": "Georgia", "portal": "GDOT Crash Data Dashboard", "url": "https://gdot.aashtowaresafety.net/crash-data-dashboard#/", "publicly_available": "No", "notes": "Detailed data available through public records request", "last_verified": "May 2026"},
    {"state": "Hawaii", "portal": "Hawaii DOT Safe Communities Program", "url": "https://hidot.hawaii.gov/highways/safe-communites/", "publicly_available": "No", "notes": "Limited county-level summary data only"},
    {"state": "Idaho", "portal": "Idaho Crash Data Map", "url": "https://itd.numetric.net/itd-safety-dashboards", "publicly_available": "Yes", "notes": "Crash attributes available for download"},
    {"state": "Illinois", "portal": "Illinois DOT Open Data", "url": "https://gis-idot.opendata.arcgis.com/search?q=Crashes", "publicly_available": "Yes", "notes": "GIS layers available for download", "last_verified": "May 2026"},
    {"state": "Indiana", "portal": "ARIES Crash Data", "url": "https://www.ariesportal.com/", "publicly_available": "No", "notes": "Data may be available thought public records request", "last_verified": "May 2026"},
    {"state": "Iowa", "portal": "ICAT (Iowa Crash Analysis Tool)", "url": "https://icat.iowadot.gov/", "publicly_available": "Yes", "notes": "Tabular or Shapefile downloads available"},
    {"state": "Kansas", "portal": "KDOT Crash Statistics", "url": "https://www.ksdot.org/burTransPlan/prodinfo/accista15.asp", "publicly_available": "No", "notes": "PDF reports only"},
    {"state": "Kentucky", "portal": "Kentucky State Police Collision Analysis for the Public", "url": "http://crashinformationky.org/AdvancedSearch", "publicly_available": "Yes", "notes": "Spatial data appears to be available; query tool not functioning at time of access"},
    {"state": "Louisiana", "portal": "Louisiana Crash Data Reports", "url": "http://datareports.lsu.edu/", "publicly_available": "No", "notes": "Spatial crash data available with formal data use agreement; limited access"},
    {"state": "Maine", "portal": "Maine Public Crash Query Tool", "url": "https://mdotapps.maine.gov/MaineCrashPublic/", "publicly_available": "Limited", "notes": "Crashes can be mapped but not downloaded; additional capabilities with Advanced User login (with permission)"},
    {"state": "Maryland", "portal": "Maryland Department of State Police (MDSP)", "url": "https://analytics.mdsp.org/t/Public/views/PublicCrashData/DownloadCrashData", "publicly_available": "Yes", "notes": "Data available for tabular download", "last_verified": "May 2026"},
    {"state": "Massachusetts", "portal": "MassDOT Open Data Portal (IMPACT)", "url": "https://apps.impact.dot.state.ma.us/cdp/home", "publicly_available": "Yes", "notes": "Tabular data available for download by automated request"},
    {"state": "Michigan", "portal": "Michigan Traffic Crash Facts Data Query Tool", "url": "https://www.michigantrafficcrashfacts.org/querytool", "publicly_available": "Limited", "notes": "Data available by query, downloads may have limited attributes"},
    {"state": "Minnesota", "portal": "MnCMAT2 (Minnesota Crash Mapping Analysis Tool)", "url": "https://www.dot.state.mn.us/stateaid/mncmat2.html", "publicly_available": "Limited", "notes": "Data with login approval"},
    {"state": "Mississippi", "portal": "Mississippi Department of Public Safety", "url": "https://www.ms.gov/dps/crash_reports", "publicly_available": "No", "notes": "Individual Crash Report purchase requests only"},
    {"state": "Missouri", "portal": "STARS Reporting", "url": "https://www.mshp.dps.missouri.gov/MSHPWeb/SAC/stars_index.html", "publicly_available": "Limited", "notes": "Data available for download, limited attributes"},
    {"state": "Montana", "portal": "Montana Statistics and Data", "url": "https://www.mdt.mt.gov/publications/datastats/crashdata.shtml", "publicly_available": "Limited", "notes": "Data available for download, limited attributes"},
    {"state": "Nebraska", "portal": "Nebraska Self-Service Crash Data Portal", "url": "https://dot.nebraska.gov/safety/crash/", "publicly_available": "Limited", "notes": "Detailed data may be mapped, but not downloaded"},
    {"state": "Nevada", "portal": "Nevada Traffic Crash Data", "url": "https://www.dot.nv.gov/safety/traffic-crash-data", "publicly_available": "Limited", "notes": "Web map only; Downloadable data available by request"},
    {"state": "New Hampshire", "portal": "New Hampshire Driving Towards Zero", "url": "https://www.nhtmc.com/dashboard/safety/", "publicly_available": "No", "notes": "No data portal identified"},
    {"state": "New Jersey", "portal": "NJ DOT Crash Data", "url": "https://www.state.nj.us/transportation/refdata/accident/rawdata01-current.shtm", "publicly_available": "Limited", "notes": "Crash tables available for download by county; .txt format; limited attributes"},
    {"state": "New Mexico", "portal": "New Mexico Traffic Crash Data", "url": "http://gps.unm.edu/tru", "publicly_available": "No", "notes": "PDF reports only; detailed data available by request"},
    {"state": "New York", "portal": "New York Traffic Safety Statistical Repository (TSSR)", "url": "https://www.itsmr.org/TSSR/", "publicly_available": "No", "notes": "Summary statistics only; detailed data available with public records request"},
    {"state": "New York City", "portal": "NYC Open Data", "url": "https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95", "publicly_available": "Yes", "notes": "Data available for download, multiple formats"},
    {"state": "North Carolina", "portal": "NC Pedestrian and Bicycle Crash Data Tool", "url": "https://www.pedbikeinfo.org/pbcat_nc/", "publicly_available": "Yes", "notes": "Data available for download; enhanced for bike/ped analysis"},
    {"state": "North Carolina (Vision Zero)", "portal": "North Carolina Vision Zero Crash Query Tool", "url": "https://ncvisionzero.org/visualizations/crashquerytool/", "publicly_available": "Yes", "notes": "Tabular data available for download"},
    {"state": "North Dakota", "portal": "NDDOT Crash Dashboard", "url": "https://www.dot.nd.gov/divisions/safety/crashdashboard.htm", "publicly_available": "No", "notes": "Dashboard only"},
    {"state": "Ohio", "portal": "Ohio AASHTOWare Safety", "url": "https://ohiodot.aashtowaresafety.com/crash-query", "publicly_available": "No", "notes": "Data available to agency personnel and approved consultants", "last_verified": "May 2026"},
    {"state": "Cincinnati", "portal": "City of Cincinnati Traffic Crash Reports", "url": "https://data.cincinnati-oh.gov/Safety/Traffic-Crash-Reports-CPD-/rvmt-pkmq", "publicly_available": "Limited", "notes": "Data available for download; limited attributes", "last_verified": "May 2026"},
    {"state": "Oklahoma", "portal": "Oklahoma Highway Safety Office", "url": "https://ohso.ok.gov/crash-data2", "publicly_available": "No", "notes": "Dashboard/PDF reports only"},
    {"state": "Oregon", "portal": "Oregon Transportation Safety Data Explorer (OTSDE)", "url": "https://geo.maps.arcgis.com/apps/webappviewer/index.html?id=df0b3cdb2f1149d3bd43436bc1dd4eac", "publicly_available": "Limited", "notes": "Crashes mapped; download capability limited"},
    {"state": "Oregon (TDS)", "portal": "ODOT TDS Crash Reports", "url": "https://tvc.odot.state.or.us/tvc/", "publicly_available": "Yes", "notes": "Tabular data may be queried and downloaded"},
    {"state": "Pennsylvania", "portal": "Pennsylvania Crash Information Tool", "url": "https://crashinfo.penndot.pa.gov/PCIT/welcome.html", "publicly_available": "Yes", "notes": "Relational database tables available for download", "last_verified": "May 2026"},
    {"state": "Rhode Island", "portal": "Rhode Island Motor Vehicle Injury Data", "url": "https://health.ri.gov/data/motorvehicleinjury/", "publicly_available": "No", "notes": "Summary statistics only"},
    {"state": "South Carolina", "portal": "Highway Safety Statistical Services", "url": "https://scdps.sc.gov/ohsjp/stat_services", "publicly_available": "No", "notes": "PDF reports only"},
    {"state": "South Dakota", "portal": "South Dakota Crash Analysis Tool", "url": "https://dps.sd.gov/records/accident-records/sdcat", "publicly_available": "No", "notes": "Customizable dashboard only; no spatial data"},
    {"state": "Tennessee", "portal": "TN Department of Safety and Homeland Security", "url": "https://www.tn.gov/safety/stats/crashdata.html", "publicly_available": "No", "notes": "Limited data available to map; no downloads"},
    {"state": "Texas", "portal": "TxDOT Crash Query Tool", "url": "https://cris.dot.state.tx.us/public/Query/app/home", "publicly_available": "Yes", "notes": "Data may be queried and downloaded after creating an account", "last_verified": "May 2026"},
    {"state": "Utah", "portal": "Utah Vehicle Collisions", "url": "https://crashmapping.utah.gov/", "publicly_available": "Limited", "notes": "Data available for download; requires advanced analytic capability"},
    {"state": "Vermont", "portal": "Vermont Public Crash Data Query Tool", "url": "http://apps.vtrans.vermont.gov/CrashPublicQueryTool/", "publicly_available": "Yes", "notes": "Data available for download; limited attributes"},
    {"state": "Virginia", "portal": "VDOT Crash Analysis Tool", "url": "https://app.powerbigov.us/view?r=eyJrIjoiMjhlZjFhZDAtNTljMC00MDA1LWEyOTMtYWYwM2NiMmRiMmRkIiwidCI6IjYyMGFlNWE5LTRlYzEtNGZhMC04NjQxLTVkOWYzODZjNzMwOSJ9", "publicly_available": "Yes", "notes": "Data available for download"},
    {"state": "Richmond", "portal": "Richmond Open Data Portal", "url": "https://data.richmondgov.com/browse?", "publicly_available": "Yes", "notes": "Pedestrian and Bicycle crash datasets available for download"},
    {"state": "Washington", "portal": "Department of Transportation Crash Data", "url": "https://www.wsdot.wa.gov/mapsdata/crash/crashdata.htm", "publicly_available": "No", "notes": "Data available by request"},
    {"state": "West Virginia", "portal": "West Virginia DOT Open Data Portal", "url": "https://data-wvdot.opendata.arcgis.com/", "publicly_available": "No", "notes": "No crash information available"},
    {"state": "Wisconsin", "portal": "WisTransPortal System", "url": "https://transportal.cee.wisc.edu/services/crash-data/", "publicly_available": "Limited", "notes": "Data available by request to general public"},
    {"state": "Milwaukee", "portal": "Traffic Accident Data", "url": "https://data.milwaukee.gov/dataset/trafficaccident", "publicly_available": "Limited", "notes": "Tabular data available for download; limited attributes"},
    {"state": "Wyoming", "portal": "Wyoming DOT Crash Data", "url": "http://www.dot.state.wy.us/home/dot_safety/crash-data.html", "publicly_available": "No", "notes": "Summary PDF reports only"},
    {"state": "District of Columbia", "portal": "Open Data DC", "url": "https://opendata.dc.gov/datasets/70392a096a8e431381f1f692aaa06afd_24/explore?location=38.893760%2C-77.019147%2C11.34", "publicly_available": "Yes", "notes": "Extensive data available for download"},
]


def layout():
    df = pd.DataFrame(CRASH_DATA_SOURCES)
    
    # Create table rows with links
    table_rows = []
    for _, row in df.iterrows():
        last_verified = row.get("last_verified")
        if pd.isnull(last_verified):
            last_verified = "2021"
        table_rows.append(
            html.Tr(
                [
                    html.Td(row["state"]),
                    html.Td(
                        html.A(
                            row["portal"],
                            href=row["url"],
                            target="_blank",
                            rel="noopener noreferrer",
                            style={"color": "#0267FD", "textDecoration": "none"},
                        )
                    ),
                    html.Td(row["publicly_available"]),
                    html.Td(row["notes"]),
                    html.Td(last_verified),  # defaults to 2021 for phase 1 tool
                ]
            )
        )

    return dbc.Container(
        [
            html.H2("Public Crash Data Sources", className="mb-3"),
            html.P(
                [
                    "Below is a comprehensive list of publicly available crash data sources by state and major city. "
                    "This information can help you identify local or regional data that may be suitable for use with the SSPF. "
                    "While many governmental agencies can readily access detailed crash data from state DOTs, either through a web portal or by request, "
                    "general public/non-governmental access to spatial data necessary to utilize this tool varies widely. ",
                    "Some of the sources listed here have not been verified for since 2021, so we encourage users to verify the current availability and accessibility of data from these sources. ",
                    "If you are aware of additional data sources or have updates to the information provided here, please email us at ",
                    html.A(CONTACT_EMAIL, href=f"mailto:{CONTACT_EMAIL}")
                ],
                className="mb-4",
            ),
            html.Div(
                [
                    dbc.Table(
                        [
                            html.Thead(
                                html.Tr(
                                    [
                                        html.Th("State/Region"),
                                        html.Th("Crash Data Portal/Site"),
                                        html.Th("Geospatial Data Publicly Available"),
                                        html.Th("Notes"),
                                        html.Th("Last Verified"),
                                    ]
                                ),
                                style={
                                    "backgroundColor": "#009879",
                                    "color": "white",
                                    "textAlign": "center",
                                },
                            ),
                            html.Tbody(table_rows),
                        ],
                        bordered=True,
                        hover=True,
                        responsive=True,
                        striped=True,
                        className="mt-3",
                    )
                ],
                style={"overflowX": "auto"},
            ),
            html.Hr(className="my-5"),
        ],
        fluid=False,
        className="py-4",
    )

# # export the table as an excel file for review and download
# if __name__ == "__main__":
#     df = pd.DataFrame(CRASH_DATA_SOURCES)
#     df.to_excel("public_crash_data_sources.xlsx", index=False)