# Version history

## 0.5.0
- Bugs fixed:
  - Column renaming in `renameColumnsMap`. Missnaming occurred when new column names had missing values or empty strings.
  - Attributes persist after operations on data frames.

- Features added:
  - `getDrugIndications()` to derive probable indications for drug dispensations.
  - `standardizeColumns()` can be used to standardize all variables in a data.frame.
  - `patientRegister` is a new register type.

- Various:
  - Improved test coverage.

## 0.4.0
- Changed to flexible way of storing exclusion data.
- Added `joinLeftPrefix()` to join data based on character prefix.
- Added `indicateEventsWithin()` to mark events occuring within a date window.
- Various bugfixes, documentation and improvements.

## 0.3.1
- Functioning `dataRegister` classes.
- Bugfix in `expandRange` #33
