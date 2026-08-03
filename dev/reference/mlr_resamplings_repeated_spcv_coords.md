# (sperrorest) Repeated coordinate-based k-means clustering

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

&nbsp;

- `repeats` (`integer(1)`)  
  Number of repeats.

## References

Brenning A (2012). “Spatial cross-validation and bootstrap for the
assessment of prediction rules in remote sensing: The R package
sperrorest.” In *2012 IEEE International Geoscience and Remote Sensing
Symposium*.
[doi:10.1109/igarss.2012.6352393](https://doi.org/10.1109/igarss.2012.6352393)
.

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingRepeatedSpCVCoords`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingRepeatedSpCVCoords$new()`](#method-ResamplingRepeatedSpCVCoords-new)

- [`ResamplingRepeatedSpCVCoords$folds()`](#method-ResamplingRepeatedSpCVCoords-folds)

- [`ResamplingRepeatedSpCVCoords$repeats()`](#method-ResamplingRepeatedSpCVCoords-repeats)

- [`ResamplingRepeatedSpCVCoords$instantiate()`](#method-ResamplingRepeatedSpCVCoords-instantiate)

- [`ResamplingRepeatedSpCVCoords$clone()`](#method-ResamplingRepeatedSpCVCoords-clone)

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

    ResamplingRepeatedSpCVCoords$new(id = "repeated_spcv_coords")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `folds()`

Translates iteration numbers to fold number.

#### Usage

    ResamplingRepeatedSpCVCoords$folds(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `repeats()`

Translates iteration numbers to repetition number.

#### Usage

    ResamplingRepeatedSpCVCoords$repeats(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingRepeatedSpCVCoords$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingRepeatedSpCVCoords$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tsk("diplodia")

# Instantiate Resampling
rrcv = rsmp("repeated_spcv_coords", folds = 3, repeats = 5)
rrcv$instantiate(task)

# Individual sets:
rrcv$iters
#> [1] 15
rrcv$folds(1:6)
#> [1] 1 2 3 1 2 3
rrcv$repeats(1:6)
#> [1] 1 1 1 2 2 2

# Individual sets:
rrcv$train_set(1)
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  32  33  41  42  43  46  54  55  56  57  58
#>  [37]  67  68  69  70  73  89  91  92  93  94  95  96  97 104 105 106 108 113
#>  [55] 114 115 121 122 124 128 129 130 131 132 133 134 135 136 137 138 139 140
#>  [73] 141 142 143 144 145 146 147 148 151 152 153 154 155 156 157 158 159 179
#>  [91] 180 181 182 183 184 185 186 187 188 189 190 222 223 224 225 227 228 229
#> [109] 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247
#> [127] 248 249 250 251 271 272 273 288 289 290 291 292 294 295 296 302 303 304
#> [145] 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322
#> [163] 323 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342
#> [181] 343 356 357 419 420 421 422 423 426 427 428 429 430 431 432 433 434 435
#> [199] 436 437 446 447 457 458 459 460 461 466 475 476 477 479 481 482 483 484
#> [217] 488 489 494 498 499 500 501 502 507 508 546 547 548 549 550 551 581 583
#> [235] 584 585 586 587 588 590 591 592 593 594 595 596 599 600 601 602 603 608
#> [253] 609 610 611 612 613 621 623 624 625 640 641 642 643 644 651 652 653 657
#> [271] 658 659 660 661 662 663 664 665 666 667 668 669 670 671 672 673 674 676
#> [289] 677 678 679 680 681 685 686 687 689 690 691 693 694 695 696 698 699 700
#> [307] 702 704 705 706 707 708 709 710 711 712 713 714 715 716 717 718 727 728
#> [325] 729 730 731 750 751 752 758 759 760 761 762 763 766 769 773 774 775 776
#> [343] 777 780 781 787 788 793 798 799 800 808 809 810 811 812 817 818 819 821
#> [361] 827 828 829 830 831 833 835 836 843 845 849 850 856 857 864 866 867 868
#> [379] 870 871 874 876 877 878 880 882 883 888 889 891 896 901 902 905 915 916
#> [397] 921 922  26  27  28  29  30  31  34  35  36  37  38  39  40  44  45  47
#> [415]  48  49  50  51  52  53  59  60  61  62  63  64  65  66  71  72  74  75
#> [433]  76  77  78  79  80  81  82  83  84  85  86  87  88  90 100 101 102 103
#> [451] 107 109 110 111 112 116 117 118 119 120 125 126 127 149 150 160 161 162
#> [469] 163 165 166 167 168 169 170 171 172 173 174 175 176 177 178 191 192 193
#> [487] 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 212
#> [505] 214 215 216 217 218 219 220 221 226 254 255 256 257 258 259 260 261 262
#> [523] 263 264 265 266 267 268 269 270 274 275 276 277 278 279 280 281 282 283
#> [541] 284 285 286 287 293 297 298 299 300 301 324 325 344 348 349 350 351 352
#> [559] 353 354 355 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372
#> [577] 373 374 375 376 377 378 379 380 381 382 383 384 385 386 387 388 389 390
#> [595] 391 392 393 394 395 396 397 398 399 400 401 402 403 404 405 406 407 408
#> [613] 409 410 411 412 413 414 415 416 417 418 424 425 441 442 443 444 445 451
#> [631] 452 453 454 462 463 464 465 467 468 469 470 471 472 473 474 478 480 485
#> [649] 486 487 491 493 496 497 509 510 511 512 513 589 598 605 606 607 614 615
#> [667] 616 619 620 654 655 656 675 682 683 684 688 692 753 754 755 756 757 764
#> [685] 765 768 771 772 778 785 789 802 820 841 875 908 909 910 911 912 913 914
rrcv$test_set(1)
#>   [1]  98  99 123 164 211 213 252 253 345 346 347 438 439 440 448 449 450 455
#>  [19] 456 490 492 495 503 504 505 506 514 515 516 517 518 519 520 521 522 523
#>  [37] 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539 540 541
#>  [55] 542 543 544 545 552 553 554 555 556 557 558 559 560 561 562 563 564 565
#>  [73] 566 567 568 569 570 571 572 573 574 575 576 577 578 579 580 582 597 604
#>  [91] 617 618 622 626 627 628 629 630 631 632 633 634 635 636 637 638 639 645
#> [109] 646 647 648 649 650 697 701 703 719 720 721 722 723 724 725 726 732 733
#> [127] 734 735 736 737 738 739 740 741 742 743 744 745 746 747 748 749 767 770
#> [145] 779 782 783 784 786 790 791 792 794 795 796 797 801 803 804 805 806 807
#> [163] 813 814 815 816 822 823 824 825 826 832 834 837 838 839 840 842 844 846
#> [181] 847 848 851 852 853 854 855 858 859 860 861 862 863 865 869 872 873 879
#> [199] 881 884 885 886 887 890 892 893 894 895 897 898 899 900 903 904 906 907
#> [217] 917 918 919 920
intersect(rrcv$train_set(1), rrcv$test_set(1))
#> integer(0)

# Internal storage:
rrcv$instance # table
#>       row_id   rep  fold
#>        <int> <int> <int>
#>    1:      1     1     2
#>    2:      2     1     2
#>    3:      3     1     2
#>    4:      4     1     2
#>    5:      5     1     2
#>   ---                   
#> 4606:    918     5     1
#> 4607:    919     5     1
#> 4608:    920     5     1
#> 4609:    921     5     3
#> 4610:    922     5     3
```
