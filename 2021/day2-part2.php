<?php

$input = explode("\n", file_get_contents('.\input.txt'));
$aim = 0;
$posX = 0;
$depth = 0;

foreach ($input as $row) {
    $split = explode(" ", $row);
    switch ($split[0]) {
        case 'down':
            $aim += $split[1];
            break;
        case 'up':
            $aim -= $split[1];
            break;
        case 'forward':
            $posX += $split[1];
            $depth += $aim * $split[1];
            break;
    }
}

print_r($posX * $depth);

?>