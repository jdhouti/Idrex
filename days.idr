module days

public export
data Days = Mon | Tues | Wed | Thu | Fri | Sat | Sun

export
tomorrow: Days -> Days
tomorrow Mon = Tues
tomorrow Tues = Wed
tomorrow Wed = Thu
tomorrow Thu = Fri
tomorrow Fri = Sat
tomorrow Sat = Sun
