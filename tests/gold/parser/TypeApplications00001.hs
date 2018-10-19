module TypeApplications00001 where


foo :: ((Maybe Int))

foo = bar x X baz $ \spam -> eggs

foo = bar @_ @X baz $ \spam -> eggs
