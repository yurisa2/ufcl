<?php
// Report all PHP errors
error_reporting(E_ALL);
// phpinfo();
$PDO = new PDO('sqlite:db/db.sqlite');
$PDO->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

$id = $_GET['id'];
$mse = $_GET['mse'];

$result = $_POST['result'];


$stmt =   "UPDATE results set status = $status where id = $id";
$query = $PDO->query($stmt);

$stmt2 =   "UPDATE results set status = $status where id = $id";
$query = $PDO->query($stmt2);

$stmt3 =   "UPDATE results set mse = $mse where id = $id";
$query = $PDO->query($stmt3);

$stmt4 =   "UPDATE results set data = $result where id = $id";
$query = $PDO->query($stmt4);

// echo $stmt; exit;



?>
