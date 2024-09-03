#!/usr/bin/env python3

output = ["%s%s" % (" "*(9-i), "".join(str(num) for num in list(range(1, i+1)) + list(range(1, i+1))[-2::-1])) for i in range(1, 10)]
print("\n".join(output + output[-2::-1]))
