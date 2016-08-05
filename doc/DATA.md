# Data Definition

This file contains data definitions for specific data sets in the tool.

## Kundalini Profile Survey (KPS)

Variable definitions, specifically for all likert-scaled variables can be found in the `data/kps1-variables.csv` data file. In addition to these descriptions, the KPS survey contains a number of "gate" questions which control which questions are administered to survey participants.

### KPS Gate Questions

- **pe.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences, have you had any physical experiences or symptoms including movements, conditions, sensations, feelings, or behaviors?_
  - Control: Opens gate to all physical experience questions, however, other gates do apply. These categories include `invmov`, `sensation`, `negphysical`, and `otherphysical`.
- **pe.invmov.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences, have you had any involuntary movements?_
  - Control: This gate is only displayed if `pe.gate` is yes. If this is answered yes, all `invmov` questions are displayed.
- **pe.sensation.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences, have you had any physical feelings and or sensations?_
  - Control: This gate is only displayed if `pe.gate` is yes. If this is answered yes, all `sensation` questions are displayed.
- **pe.negphysical.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences, have you had any physical illness or discomfort directly related to your mystical or spiritual experience?_
  - Control: This gate is only displayed if `pe.gate` is yes. If this is answered yes, all `negphysical` questions are displayed.
- **psygrowth.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences, have you had any psychological growth that increased the value of your life in a personal way?_
  - Control: If this is answered yes, all `psygrowth` questions are displayed.
- **negpsych.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences did you or are you going through negative psychological efftects?_
  - Control: If this is answered yes, all `negpsych` questions are displayed.
- **psybliss.gate**
  - Question text: _DIRECTLY RELATED to one or more of your mystical or spiritual experiences, are you finding life to be more joyful with ease?_
  - Control: If this is answered yes, all `psybliss` questions are displayed.