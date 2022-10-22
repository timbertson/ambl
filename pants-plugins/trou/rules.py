from pants.engine.console import Console
from pants.engine.target import Targets
from pants.engine.goal import Goal, GoalSubsystem
from pants.engine.rules import collect_rules, goal_rule

from trou.targets import BuilderField

class HelloWorldSubsystem(GoalSubsystem):
    name = "hello-world"
    help = "An example goal."


class HelloWorld(Goal):
    subsystem_cls = HelloWorldSubsystem

@goal_rule
async def hello_world(targets: Targets, console: Console) -> HelloWorld:
    console.print_stdout("Hello!")

    for tgt in targets:
        console.print_stdout(repr(tgt))
        if tgt.has_field(BuilderField):
            console.print_stdout("BUILDER: %r - %s" % (tgt, tgt[BuilderField].value))

    return HelloWorld(exit_code=0)
 

def get_rules():
    return collect_rules()
