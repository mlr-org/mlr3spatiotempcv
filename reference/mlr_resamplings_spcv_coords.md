# (sperrorest) Coordinate-based k-means clustering

Splits data by clustering in the coordinate space. See the upstream
implementation at
[`sperrorest::partition_kmeans()`](https://giscience-fsu.github.io/sperrorest/reference/partition_kmeans.html)
and Brenning (2012) for further information.

## Details

Universal partitioning method that splits the data in the coordinate
space. Useful for spatially homogeneous datasets that cannot be split
well with rectangular approaches like `ResamplingSpCVBlock`.

## Parameters

- `folds` (`integer(1)`)  
  Number of folds.

## References

Brenning A (2012). “Spatial cross-validation and bootstrap for the
assessment of prediction rules in remote sensing: The R package
sperrorest.” In *2012 IEEE International Geoscience and Remote Sensing
Symposium*.
[doi:10.1109/igarss.2012.6352393](https://doi.org/10.1109/igarss.2012.6352393)
.

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingSpCVCoords`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingSpCVCoords$new()`](#method-ResamplingSpCVCoords-new)

- [`ResamplingSpCVCoords$instantiate()`](#method-ResamplingSpCVCoords-instantiate)

- [`ResamplingSpCVCoords$clone()`](#method-ResamplingSpCVCoords-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Create an "coordinate-based" repeated resampling instance.

For a list of available arguments, please see
[sperrorest::partition_cv](https://giscience-fsu.github.io/sperrorest/reference/partition_cv.html).

#### Usage

    ResamplingSpCVCoords$new(id = "spcv_coords")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingSpCVCoords$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingSpCVCoords$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tsk("ecuador")

# Instantiate Resampling
rcv = rsmp("spcv_coords", folds = 5)
rcv$instantiate(task)

# Individual sets:
rcv$train_set(1)
#>   [1]   2   4   8  15  18  24  25  26  28  32  48  50  63  64  70  85  89  97
#>  [19] 102 104 118 120 129 131 135 138 150 151 153 158 161 166 172 192 200 206
#>  [37] 207 214 215 216 217 221 224 227 234 238 246 251 255 261 265 266 268 271
#>  [55] 272 278 279 280 281 287 288 306 313 344 346 348 354 358 362 370 373 377
#>  [73] 388 402 406 411 412 422 423 426 428 429 430 433 435 438 451 455 460 461
#>  [91] 468 470 476 480 488 499 512 518 525 529 532 541 543 555 558 559 561 562
#> [109] 563 582 586 589 592 598 607 611 617 623 633 651 656 683 693 707 712 726
#> [127] 727 730 737 738 746   7  12  13  14  17  21  35  44  46  54  65  66  75
#> [145]  78  79  86  88  91  94 105 109 113 123 128 130 132 137 140 142 143 145
#> [163] 147 162 163 171 181 182 188 191 195 196 202 204 209 211 213 223 225 229
#> [181] 240 242 253 259 262 270 277 282 285 305 308 318 320 322 327 331 336 337
#> [199] 338 359 365 366 368 371 378 381 384 395 409 416 418 431 446 448 452 458
#> [217] 465 467 472 475 477 478 489 493 494 496 498 505 511 527 533 535 538 542
#> [235] 547 554 565 566 567 570 580 581 583 591 599 602 618 619 621 631 636 638
#> [253] 646 658 663 667 668 670 671 680 681 684 691 701 703 705 714 720 723 734
#> [271] 740 747 750   1   6  11  16  23  33  34  36  47  51  52  56  59  60  76
#> [289]  84  87  93  95  96  99 100 107 111 112 117 121 139 144 146 148 149 152
#> [307] 156 157 164 165 167 168 174 175 178 184 193 198 210 212 232 239 241 248
#> [325] 249 250 252 256 257 258 264 267 269 273 274 283 286 294 295 297 300 302
#> [343] 307 310 312 314 316 319 321 324 329 330 332 342 347 351 361 363 364 369
#> [361] 374 375 376 379 382 387 391 393 397 399 407 408 410 417 419 420 421 424
#> [379] 425 432 439 440 445 447 459 466 469 471 473 481 482 485 491 497 501 504
#> [397] 507 514 515 516 517 534 540 545 548 553 560 569 571 572 577 588 590 604
#> [415] 609 624 626 629 630 632 635 637 640 645 647 650 660 665 666 669 674 676
#> [433] 677 678 686 689 695 698 699 700 702 704 716 721 731 742 745 748 749 751
#> [451]   3   5  10  19  29  37  39  42  45  49  55  57  58  61  62  68  77  90
#> [469] 103 106 110 115 122 125 126 127 133 136 155 169 170 183 190 201 203 218
#> [487] 219 220 222 230 231 236 243 245 260 263 289 290 303 309 317 325 345 349
#> [505] 350 356 357 372 383 385 394 400 403 404 405 413 414 427 442 444 449 453
#> [523] 463 464 474 483 486 487 490 492 502 503 509 510 523 528 531 537 539 544
#> [541] 546 550 552 557 564 573 575 578 585 594 596 613 620 627 628 639 641 642
#> [559] 643 644 649 652 653 654 655 657 661 662 664 673 682 688 690 692 696 706
#> [577] 708 710 711 715 718 724 728 729 732 733 735 736 739 741 743 744
rcv$test_set(1)
#>   [1]   9  20  22  27  30  31  38  40  41  43  53  67  69  71  72  73  74  80
#>  [19]  81  82  83  92  98 101 108 114 116 119 124 134 141 154 159 160 173 176
#>  [37] 177 179 180 185 186 187 189 194 197 199 205 208 226 228 233 235 237 244
#>  [55] 247 254 275 276 284 291 292 293 296 298 299 301 304 311 315 323 326 328
#>  [73] 333 334 335 339 340 341 343 352 353 355 360 367 380 386 389 390 392 396
#>  [91] 398 401 415 434 436 437 441 443 450 454 456 457 462 479 484 495 500 506
#> [109] 508 513 519 520 521 522 524 526 530 536 549 551 556 568 574 576 579 584
#> [127] 587 593 595 597 600 601 603 605 606 608 610 612 614 615 616 622 625 634
#> [145] 648 659 672 675 679 685 687 694 697 709 713 717 719 722 725
# check that no obs are in both sets
intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#> integer(0)

# Internal storage:
rcv$instance # table
#> Key: <fold>
#>      row_id  fold
#>       <int> <int>
#>   1:      9     1
#>   2:     20     1
#>   3:     22     1
#>   4:     27     1
#>   5:     30     1
#>  ---             
#> 747:    736     5
#> 748:    739     5
#> 749:    741     5
#> 750:    743     5
#> 751:    744     5
```
