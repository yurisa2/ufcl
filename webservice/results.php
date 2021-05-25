<?php
// Report all PHP errors
error_reporting(E_ALL);
// phpinfo();
$PDO = new PDO('sqlite:db/db.sqlite');
$PDO->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

$id = $_GET['id'];
$mse = $_POST['mse'];

$result = $_POST['result'];

// file_put_contents($id."rest1.txt", json_encode($_POST));

$stmt =   "INSERT INTO results (paramId, mse, data) values ('$id','$mse','$result')";
$query = $PDO->query($stmt);

$stmt2 =   "UPDATE params set status = 'FINISHED' where id = $id";
$query = $PDO->query($stmt2);

// file_put_contents($id."query.txt", json_encode($query));


// echo $stmt; exit;



?>
