<?php
// Report all PHP errors
error_reporting(E_ALL);
// phpinfo();
$PDO = new PDO('sqlite:db/db.sqlite');
$PDO->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

if(empty($_GET['id'])) {
  $id = NULL;
  $stmt =   "SELECT * FROM params where status IS NULL limit 1";
  $query = $PDO->query($stmt);
  $result = $query->fetchall();
  $id = $result[0]['id'];

  $stmt2 =  "UPDATE params set status = 'delivered' where id = $id";
    // echo $stmt2;
  $timeNow = time();
  $stmt3 =  "UPDATE params set first_interaction = $timeNow where id = $id";

  $up1 = $PDO->query($stmt2);
  $up2 = $PDO->query($stmt3);
}
else {
  $id = $_GET['id'];
  $stmt =   "SELECT * FROM params where id = " . "'" .$id. "'";
  $query = $PDO->query($stmt);
  $result = $query->fetchall();
}

// echo "Oi24";
//
// var_dump($stmt);
//
// echo '<pre>';


// var_dump($result);

$toCsv = $result[0];

// var_dump($toCsv);

echo $id,',',$toCsv['intervals'],',',$toCsv['order'],',',$toCsv['method'];



 ?>
