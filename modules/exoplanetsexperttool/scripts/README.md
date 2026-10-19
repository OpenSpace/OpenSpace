# Dataset Settings

Dataset settings JSON files describe how the Exoplanet Expert Tool should interpret a CSV. An example in this folder is `datasettings_exoplanets.json`.

## Minimal Example

```json
{
  "datafile": "data/my_dataset.csv",
  "data_mapping": {
    "position_ra": "ra_degrees",
    "position_dec": "dec_degrees",
    "position_distance": "distance_pc",
    "name": "object_name",
    "hostName": "host_name"
  }
}
```

The values on the right are exact column names from the CSV header. Names are case-sensitive. The selected CSV must have a header row, unique non-empty column names, and the same number of fields on every data row.

When opening a CSV in the application, the CSV path is required and the settings JSON path is optional. The selected CSV path takes precedence over `datafile` in the JSON. If no JSON is supplied, the application starts with generic settings and asks you to map the required columns. Settings can omit `datafile` when they are supplied alongside an explicitly selected CSV.

## `data_mapping`

This object maps CSV columns to roles used by the tool:

| JSON key | Required | Meaning |
| --- | --- | --- |
| `position_ra` | Required for equatorial mode | Right ascension, numeric, in degrees. |
| `position_dec` | Required for equatorial mode | Declination, numeric, in degrees. |
| `position_distance` | Required for equatorial mode | Numeric radial distance in `position_unit` (defaults to parsecs). |
| `position_x`, `position_y`, `position_z` | Required for Galactic Cartesian mode | Galactic Cartesian coordinates; all three numeric columns use `position_unit`. |
| `position_type` | No | Coordinate representation: `"equatorial"` (the default) or `"galactic_cartesian"`. |
| `position_unit` | No | Unit of the equatorial distance value or all three Galactic Cartesian components. Defaults to `"Parsec"`. Accepts a supported distance-unit name or abbreviation, such as `"Lightyear"`, `"ly"`, `"AU"`, `"kpc"`, or `"m"`. |
| `name` | No | Object display-name column. If omitted or unavailable, the application asks you to choose a column. You can instead select **Use row index** in the mapping window. That produces 1-based names in CSV data-row order. |
| `hostName` | No | Host-star name. Rows with a non-empty host value are grouped in the system view; without a usable host, opening the system view shows only the selected object. |
| `ring_size` | No | Numeric size used to order objects within a host system, where applicable. |
| `reference_name` | No | Reference label associated with an object. |
| `reference_link` | No | Reference URL associated with an object. |
| `metallicity_ratio` | No | Column used for metallicity-ratio presentation in the system view. |
| `discovery_method` | No | Discovery-method column used in the system view. |
| `inclination` | No | Orbital inclination column, in degrees. |
| `stellar_inclination` | No | Stellar inclination column, in degrees. |

Choose one coordinate representation. Equatorial mode requires `position_ra`, `position_dec`, and `position_distance`; RA and Dec are in degrees. Galactic Cartesian mode requires `position_x`, `position_y`, and `position_z`. The distance column or all three Cartesian components must use the selected `position_unit`. The unit defaults to parsecs for compatibility with existing settings files.

The coordinate selectors and unit can also be chosen in the CSV loading window. Position values are converted to parsecs internally, regardless of the selected input unit. Each required mapping must refer to a CSV column containing numeric values, and at least one row must have valid numeric values for all three coordinates. Other rows may have missing or invalid positions; those rows can still appear in the data table but do not have a rendered 3D position.

Example settings for Galactic Cartesian input:

```json
"data_mapping": {
  "position_type": "galactic_cartesian",
  "position_x": "gal_x",
  "position_y": "gal_y",
  "position_z": "gal_z",
  "position_unit": "Lightyear",
  "name": "object_name"
}
```

The object-name column is optional. To use row-index names, choose that option in the application's column-mapping window; it is an interactive choice rather than a JSON `data_mapping` key. Generated index names do not replace any existing CSV column.

## Optional Settings

### `system_view`

Lists columns to show in the grouped system overview. Each entry must be an array of CSV column names. Missing columns are ignored.

```json
"system_view": {
  "system_columns": ["number_of_stars"],
  "star_columns": ["stellar_temperature", "stellar_radius"],
  "planet_columns": ["planet_radius", "orbital_period"]
}
```

Without a host mapping, the system view is a single-object detail view and displays the available columns for that object.

### `default_colormapping`

Selects the initially active color-mapping column and its numeric range:

```json
"default_colormapping": {
  "column": "planet_radius",
  "min": 0,
  "max": 20
}
```

The selected column must exist. If it does not, the application ignores this default and selects an available column instead. Use `{}` or omit the section to let the application choose a default.

### `column_info`

Provides display metadata keyed by CSV column name. The `name` value is required for each entry; the other fields are optional.

```json
"column_info": {
  "planet_radius": {
    "name": "Radius",
    "format": "%.2f",
    "desc": "Planet radius in Earth radii",
    "isText": false
  },
  "catalogue_id": {
    "name": "Catalogue ID",
    "isText": true
  }
}
```

- `name`: label shown in the interface.
- `format`: printf-style format used for numeric display, such as `"%.0f"` or `"%.2f"`.
- `desc`: explanatory text shown in tooltips.
- `isText`: optionally force a non-position column to be treated as text. Name, host, and reference mappings are treated as text automatically. Position columns remain numeric.

Metadata for columns not present in the CSV is ignored. Unconfigured columns are still available to the table and other views.

### `quick_filters`

An optional array of groups that define preset filters. Each group has a title, an `AND` or `OR` type, an optional `per_line` layout value, and a `filters` array. Each filter has a display `name`, a `filter` value containing one clause or an array of clauses, and an optional description.

```json
"quick_filters": [
  {
    "group_title": "Size",
    "type": "OR",
    "per_line": 2,
    "filters": [
      {
        "name": "Small",
        "filter": { "column": "planet_radius", "query": "<= 1.5" },
        "desc": "Radius at most 1.5 Earth radii"
      },
      {
        "name": "Medium",
        "filter": [
          { "column": "planet_radius", "query": "> 1.5" },
          { "column": "planet_radius", "query": "<= 4.0" }
        ]
      }
    ]
  }
]
```

A single-clause `filter` may be an object; multiple clauses use an array. Each clause names a CSV `column` and a `query`. If a preset group refers to a column that is missing, that group is ignored rather than partially applied. Omit `quick_filters` or use `[]` when no presets are needed.

## Paths and Missing Columns

`datafile` may use an OpenSpace path token such as `${MODULE_EXOPLANETSEXPERTTOOL}/data/example.csv`. Ordinary relative paths in a settings file are resolved relative to that JSON file. When a CSV is explicitly selected in Open CSV, that selected path is used.

Required position mappings that do not match the CSV header are presented for correction in the mapping window. Missing optional mappings and metadata are ignored, and missing optional system-view columns are omitted. A supplied settings JSON that cannot be parsed is reported as an error; it is not silently replaced with generic settings.
