<?php

$input = explode("\n", file_get_contents('.\input.txt'));
$gamma = '';
$epsilon = '';
$bitSum = 0;

for ($i = 0; $i < 12; $i++) {
    for ($j = 0; $j < count($input); $j++) {
        if ($input[$j][$i] == 0) {
            $bitSum -= 1;
        } else {
            $bitSum += 1;
        }
    }
    
    if ($bitSum < 0) {
        $gamma .= '0';
        $epsilon .= '1';
    } else {
        $gamma .= '1';
        $epsilon .= '0';
    }

    $bitSum = 0;
}

print_r(bindec($gamma) * bindec($epsilon));

?>