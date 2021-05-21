<?php
// Report all PHP errors
error_reporting(E_ALL);
// phpinfo();
$PDO = new PDO('sqlite:db/db.sqlite');
$PDO->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

$id = $_GET['id'];
$status = $_GET['status'];

$stmt =   "UPDATE params set status = $status where id = $id";

// echo $stmt; exit;

$query = $PDO->query($stmt);


?>
