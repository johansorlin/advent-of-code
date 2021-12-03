<?php

$input = explode("\n", file_get_contents('.\input.txt'));

function determineRate($input, $ind) {
    $bitSum = 0;
    $trueBits = array();
    $falseBits = array();

    for ($i = 0; $i < 12; $i++) {
        for ($j = 0; $j < count($input); $j++) {
            if ($input[$j][$i] == $ind) {
                if ($ind == 0) {
                    $bitSum -= 1;
                    array_push($falseBits, $j);
                } else {
                    $bitSum -= 1;
                    array_push($trueBits, $j);
                }
            } else {
                if ($ind == 0) {
                    $bitSum += 1;
                    array_push($trueBits, $j);
                } else {
                    $bitSum += 1;
                    array_push($falseBits, $j);
                }
            }
        }

        if ($bitSum < 0) {
            for ($k = 0; $k < count($trueBits); $k++) {
                unset($input[$trueBits[$k]]);
            }
        } elseif($bitSum > 0) {
            for ($k = 0; $k < count($falseBits); $k++) {
                unset($input[$falseBits[$k]]);
            }
        } else { // bitSum = 0 -> true and false bits are equally common
            if ($ind == 0) {
                // indicator is 0 (looking for oxygen), remove false bits
                for ($k = 0; $k < count($falseBits); $k++) {
                    unset($input[$falseBits[$k]]);
                }
            } else {
                // indicator is 1 (looking for CO2), remove true bits
                for ($k = 0; $k < count($trueBits); $k++) {
                    unset($input[$trueBits[$k]]);
                }
            }
        }

        $input = array_values($input);
        $falseBits = array();
        $trueBits = array();
        $bitSum = 0;

        if (count($input) == 1) {
            return $input[0];
            break;
        }
    }
}

print_r(bindec(determineRate($input, 0)) * bindec(determineRate($input, 1)));

?>
