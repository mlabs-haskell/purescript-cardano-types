import * as CDL from "@mlabs-haskell/cardano-data-lite";

export function defaultCostmdls() {
  return CDL.TxBuilderConstants.plutus_vasil_cost_models();
}
