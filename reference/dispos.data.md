# Demo clinical trial disposition data

This is a demo data to demonstrate the use of the package. One row per
participant. Participants who are excluded should provide a reason,
missing otherwise.

## Usage

``` r
dispos.data
```

## Format

A data frame with 300 rows and 11 columns:

- trialno:

  Participants ID of the participants

- exclusion1, exclusion2:

  Exclusion reason before and after induction

- induction:

  Participants ID of the participants who are included in the induction
  phase, an extra treatment before randomisation.

- exclusion:

  Exclusion reason before randomisation, including before and after
  induction

- arm, arm3:

  Arms pariticipants randomised to.

- sbujid_dosed:

  Participants ID of the participants who had at least one dose of the
  protocol treatment.

- subjid_notdosed:

  Reason for participants not dosed.

- followup:

  Participants ID planned for follow-up.

- lost_followup:

  Reason for participants not dosed.

- assessed:

  Participants ID participants attended assessment.

- no_value:

  Reason for participants missing final assessment.

- mitt:

  Participants ID included in the mITT analysis.
