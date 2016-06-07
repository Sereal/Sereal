class SrlError(Exception):
    def __init__(self, arg):
        super(SrlError, self).__init__()
        self.arg = arg

    def __str__(self):
        return repr(self.arg)