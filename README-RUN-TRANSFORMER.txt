Steps for fitting the transformer model:

1. Run `scripts/make-trajectory-smt-data.R`.
  - Run time will be about a minutes.
  - The number of parallel processes by adjusting the 
    `workers` parameter value on line 6.
  - The output will be a new file `data/smt-data.rds`.

2. Run `scripts/smt.R`.
  - Run time will probably be about 1 day with 1000 epochs.
  - The number of epochs can be adjusted on line 8.
  - Tensors are created for training "on the fly" in the `.getitem()` method
    on line 21. You may see much better performance by pre-creating the
    tensor data. However, this will take take more memory (probably around
    30GB). 
