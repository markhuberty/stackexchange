<?php

require 'BatchXMLReader.php';


final class MySQL_Importer implements BatchProcessor {

    const HOSTNAME = 'localhost';
    const USERNAME = 'markhuberty';
    const PASSWORD = 'overflowpwd';
    const DATABASE = 'stackoverflow';
    const ENCODING = 'utf8';

    private static $con;
    private $count  = 0;
    private $diff_t = 0;
    private $start_t;
    private $table;
    private $columns;
    private $col_str;


    function __construct($file, $table, $columns) {
        echo sprintf("Parsing XML file \"%s\" and importing into `%s`.`%s`\n",
            $file, self::DATABASE, $table
        );

        set_error_handler(function ($error_num, $error_msg) {
            echo $error_msg;
            error_log(' - ' . $error_msg); exit;
        });

        $this->start_t = time();
        $this->table   = $table;
        $this->columns = $columns;
        $this->col_str = '`' . implode('`, `', $columns) . '`';
        $this->connect();

        new BatchXMLReader($file, $this);

        echo sprintf(" - successfully imported %u rows in %u seconds\n\n",
            $this->count, time() - $this->start_t
        );
    }


    function process(&$array) {
        $sql = "INSERT INTO `$this->table` ($this->col_str) VALUES\n";

        foreach ($array as $key => $row) {
            $sql .= ($key ? ",\n" : "") . $this->format_row($row);
        }

        if (!mysqli_query(self::$con, $sql)) {
            trigger_error(mysqli_error(self::$con));
        }

        $this->count += count($array);
        
        if ($this->count % 500000 === 0) {
            echo sprintf(" - %02.1f million imported after %u seconds...\n",
                $this->count / 1000000,
                time() - $this->start_t
            );
        }
    }


    private function connect() {
        self::$con = mysqli_connect(
            self::HOSTNAME,
            self::USERNAME,
            self::PASSWORD
        );

        if (!self::$con) {
            trigger_error(mysqli_connect_error());
        }

        if (!mysqli_select_db(self::$con, self::DATABASE)) {
            trigger_error(mysqli_error(self::$con));
        }

        if (!mysqli_set_charset(self::$con, self::ENCODING)) {
            trigger_error(mysqli_error(self::$con));
        }

        if (!mysqli_query(self::$con, "TRUNCATE TABLE `$this->table`;")) {
            trigger_error(mysqli_error(self::$con));
        }
    }


    private function format_row($row) {
        $new = array_fill_keys($this->columns, "NULL");

        foreach ($row as $key => $val) {
            if (!is_numeric($val)) {
                $val = "'" . mysqli_real_escape_string(self::$con, $val) . "'";
            }

            $new[$key] = $val;
        }

        return '(' . implode(', ', $new) . ')';
    }
}


//new MySQL_Importer('../badges.xml', 'badges', array(
                                                    //    'ID',
    //    'USERID',
    //    'NAME',
    //    'DATE'
    //));


new MySQL_Importer('../users_new.xml', 'users', array(
    'ID',
    'REPUTATION',
    'CREATIONDATE',
    'DISPLAYNAME',
    'EMAILHASH',
    'LASTACCESSDATE',
    'WEBSITEURL',
    'LOCATION',
    'AGE',
    'ABOUTME',
    'VIEWS',
    'UPVOTES',
  'DOWNVOTES'
));
/*
new MySQL_Importer('../comments.xml', 'comments', array(
    'ID',
    'POSTID',
    'SCORE',
    'TEXT',
    'CREATIONDATE',
    'USERID'
));

new MySQL_Importer('../posts.xml', 'posts', array(
    'ID',
    'POSTTYPEID',
    'PARENTID',
    'ACCEPTEDANSWERID',
    'CREATIONDATE',
    'SCORE',
    'VIEWCOUNT',
    'BODY',
    'OWNERUSERID',
    'LASTEDITORUSERID',
    'LASTEDITORDISPLAYNAME',
    'LASTEDITDATE',
    'LASTACTIVITYDATE',
    'COMMUNITYOWNEDDATE',
    'CLOSEDDATE',
    'TITLE',
    'TAGS',
    'ANSWERCOUNT',
    'COMMENTCOUNT',
    'FAVORITECOUNT'
));

new MySQL_Importer('../votes.xml', 'votes', array(
    'ID',
    'POSTID',
    'VOTETYPEID',
    'CREATIONDATE',
    'USERID',
    'BOUNTYAMOUNT'
));
*/
?>
