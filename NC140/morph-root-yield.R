### This script evaluates regressions for morphology, roots, and yield.

source('root-yield.csv')

above_trees <- arrange(
  filter(tree_morph, rootstock == 'Bud.9' |
           rootstock == 'CG.3041' |
           rootstock == 'CG.6210'),
  tree)

root_trees  <- arrange(
  filter(tree_totals, id > 0),
  id)