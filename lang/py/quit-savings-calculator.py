#!/usr/bin/env python3

import datetime

# Generic script to calculate savings since quitting something
# (drinking, smoking, or whatever).

# These three variables should be updated accordingly
cost_per_unit = 2
units_per_day = 10
quit_date = datetime.datetime(2020, 6, 1)

now = datetime.datetime.now()
days_since = (now - quit_date).total_seconds() / 60 / 60 / 24
money_saved = days_since * units_per_day * cost_per_unit
print(round(money_saved, 2))
