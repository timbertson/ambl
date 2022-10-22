from pants.engine.target import COMMON_TARGET_FIELDS
from pants.engine.target import Dependencies, SingleSourceField, Target, Tags
from pants.engine.rules import rule

from pants.engine.target import StringField
    
class BuilderField(StringField):
    alias = "builder"
    required = True


class TrouTarget(Target):
    alias = "trou"
    core_fields = (*COMMON_TARGET_FIELDS, BuilderField)

