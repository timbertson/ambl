from trou.targets import TrouTarget
from trou.rules import get_rules

def rules():
    return [*get_rules()]

def target_types():
    return [TrouTarget]

