<?php

$input = explode("\n", file_get_contents('.\input.txt'));
$counter = 0;

for ($i = 0; $i < count($input) - 3; $i++) {
    $lowWindow = $input[$i] + $input[$i + 1] + $input[$i + 2];
    $highWindow = $input[$i + 1] + $input[$i + 2] + $input[$i + 3];

    if ($lowWindow < $highWindow) {
        $windowSumIncreaseCount++;    
    }
}

print_r($counter)

?>