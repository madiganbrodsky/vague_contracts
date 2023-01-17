<?php

// // lookup the list of participant ids that have already been recorded
// $file_handle = fopen('results/participant_ids.txt', 'r');
// $existing_participants = fread($file_handle, filesize('results/participant_codes.txt'));
// fclose($file_handle);
// $existing_participants = explode(",", $existing_participants);

// LEGACY FEATURE: GENERATE A PARTICIPANT CODE
// session_start();
// if (empty($_SESSION['participant_id'])) {
// 	// create a random participant id 
//     $_SESSION['participant_id'] = strval(mt_rand(100000, 888888));
//     // update name of the participant id until it doesn't match anything in the list
// 	// (i.e. ensure uniqueness)
// 	while(in_array($participant_id,$existing_participants)){
//     	$participant_id = strval(mt_rand(100000, 888888));
// 	}
// 	$participant_id  = $_SESSION['participant_id'];
// } else {
//     $participant_id  = $_SESSION['participant_id'];
// }

// read the data

$trials = json_decode($_POST['trials'],true);
$system = json_decode($_POST['system'],true);
$subject_information  = json_decode($_POST['subject_information'],true);
$time_in_minutes  = $_POST['time_in_minutes'];
$participant_id = json_decode($_POST['participant_id']);

// transform the data

$result = [];
foreach ($trials as $x => $val) {
	$val["participant"] = $participant_id;
	$val["time_in_minutes"] = $time_in_minutes;
    $val = array_merge($val, $system);
	$val = array_merge($val, $subject_information);
	$result[] = $val;
}

// write, but don't overwrite, participant results file

if (!file_exists( 'results/'.$participant_id.'.json' )) {
	$file_handle = fopen('results/'.$participant_id.'.json', 'w');
	fwrite($file_handle, json_encode($result));
	fclose($file_handle);
	// write participant id to list of existing ids 
	$file_handle = fopen('results/participant_ids.txt', 'a');
	fwrite($file_handle, $participant_id.',');
	fclose($file_handle);
	echo("<b>Thanks for participating!</b> <p> You may now close the screen.");
} else {
	echo("Oops! We've already collected data from a participant with your id. (You may be seeing this message because you just refreshed the screen). <p> Please exit this window.");
}


?>

