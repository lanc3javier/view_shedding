# view_shedding
Some R code on how to ideally place a radio antenna to maximize field coverage given corn growth and slope/topography.

# 🛰️ Ag-Robot Antenna Optimization Engine

This R-based spatial engine automates the placement of telecommunications antennas for agricultural robotics. It identifies the "Ideal Spot" along a field perimeter to maximize signal coverage while accounting for terrain, crop height, and robot dimensions.

## 🛠️ How It Works
The engine performs a batch-processed viewshed analysis using the **WhiteboxTools** geospatial library.

1. **Terrain Analysis:** Downloads high-resolution DEMs and calculates slope.
2. **Candidate Sampling:** Generates potential mounting points along the field boundary.
3. **Visibility Math:** Runs a viewshed for every point, accounting for:
   - `antenna_h`: The height of your stationary base station.
   - `robot_h`: The height of the receiver on the robot.
   - `corn_h`: The biological obstruction (crop height) at peak season.
4. **Output Generation:** - **GeoPackage:** Vector layers for boundaries, signal zones (Live/Dead), and top-tier points.
   - **Coverage Map:** A high-quality PNG with hillshading, contours, and parameter labels.

## 🚀 Quick Start
1. Place your field boundaries (KML, GPKG, or SHP) into the `/input_fields` folder.
2. Open `viewshedding_tool.R` in RStudio.
3. Adjust the **Dashboard** parameters for your specific hardware.
4. Run `run_antenna_optimization()`.

## 📁 Output Structure
Each field generates a results folder containing:
- `[Field]_dem.tif`: The raw elevation data.
- `[Field]_coverage_map.png`: A zoomed topographic map for field crews.
- `[Field]_Robot_Analysis.gpkg`: The multi-layer file for QGIS or robot path-planning.

