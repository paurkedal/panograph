## v0.8.3 - 2020-10-18

- Also support `js_of_ocaml` 3.7.

## v0.8.2 - 2020-05-25

- Supplement `js_of_ocaml-ppx` with a dependency on
  `js_of_ocaml-ppx_deriving_json` due to the 3.6 release.

## v0.8.1 - 2019-11-06

- Add Unicode sanity checks for `Panui_scalar` text inputs.
- Minor code maintenance.

## v0.8.0 - 2019-01-24

- Update to `js_of_ocaml` 3.3 and Eliom 6.6.

## v0.7.0 - 2018-12-07

- Switch `Panograph_i18n.lang` to `Iso639.Lang.t`.

## v0.6.0 - 2018-06-21

- Breaking: Replace `ack` type with `Panui_result.t`.
- Add `?input_a` option to `Panui_scalar` functions.
- Add `Panui_result.error_f`.
- Remove deprecated modules `Panui_combo_selectors`, `Panui_completion`,
  `Pwt_infix` and deprecated functions from `Pwt_list`.

## v0.5.3 - 2018-02-19

- Updated to work with Eliom 6.3.0.

## v0.5.2 - 2017-08-11

- Fixed `prime.unstable` dependency in `_tags`.
- Deprecated `Panui_combo_selectors`.
- Deprecated `Panograph_basic_editors.bool_checkbox`.

## v0.5.1 - 2017-03-10

- Added abstract and richer error type in `Panui_error` and `Panui_result`.
- Added `Panui_complete` to replace `Panui_completion`.
- Deprecated most of `Panograph_basic_editors`.

## v0.5.0 - 2016-11-15

- Update to Eliom 6.0.0.

## v0.4.4 - 2016-11-12

- Fix server-client typing.

## v0.4.3 - 2016-10-25

- Added `Panreact_event_array`.
- Fixed tests.

## v0.4.2 - 2016-07-16

- Updated to prime 0.7.0.

## v0.4.1 - 2016-07-11

- Updated to latest topkg and adpkg.

## v0.4.0 - 2016-06-29

- Added `Pwt_list.flatten_map_s`, `Pwt_list.flatten_map_p`.
- Removed package aliases `panograph-client` and `panograph-server`.

## v0.3.4 - 2016-06-04

- Dropped version constraint for topkg.

## v0.3.3 - 2016-06-02

### Deprecated
- Findlib packages `panograph-client` and `panograph-server` have been
  replaced by `panograph.client` and `panograph.server`.
