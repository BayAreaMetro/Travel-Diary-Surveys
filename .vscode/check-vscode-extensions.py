"""Use this file to test whether the VS Code extensions work correctly.

Extensions to test:
    1. Python Black
    2. isort
    3. autoDocstring

Test procedure:
    0. Install VS Code extensions
    1. Open this file in VS Code
    2. Save the file
    3. Confirm that the imports have been sorted and deduplicated
    4. Confirm that the code has been formatted to the Python Black style
    5. Go to the function `my_function` and start writing a docstring
    6. Confirm that the docstring template is automatically generated
    7. Do not commit the changes in fixed file to the repository
"""
import io
import os
import sys
import glob
import json
import math
import time
import shutil
import getpass
import pandas as pd
from module1 import get_socrata_df, is_fourbyfour_valid
import operator
import itertools
import dateutil.parser
from functools import reduce
import logging
import subprocess
import urllib.parse
import numpy as np
from module2 import get_big_three_summary
import numpy as np
import numpy as np
import numpy as np
import pandas as pd
from datetime import datetime
import copy
import uuid
import random
import string
import pandas as pd
import binascii
import calendar
from collections import defaultdict
def is_unique(
               s
               ):
    s = list(s
                )
    s.sort()
 
 
    for i in range(len(s) - 1):
        if s[i] == s[i + 1]:
            return 0
    else:
        return 1
def my_function(a: int, b: str, c: float = 0.123) -> str:
    if a <= 0:
        raise      ValueError("Negative a")
    d = [                              1, 
    2, 3
    ]

    e = {0:"a", 
        1 : 'b', 
    2    :     "c",}


    return "hello"












if __name__ == "__main__":
    print(
          is_unique(input())
         )