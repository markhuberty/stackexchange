<?php

interface BatchProcessor {
    public function process(&$array);
}


class BatchXMLReader {

    const BATCH_SIZE = 500;
    const NODE_NAME  = 'ROW';
    const CHARSET    = 'utf-8';

    private $file  = "";
    private $batch = array();
    private $parser;
    private $handler;



    function __construct($file, BatchProcessor $handler) {
        $this->file    = $file;
        $this->handler = $handler;
        $this->parser  = xml_parser_create(self::CHARSET);

        xml_set_object($this->parser, $this);
        xml_set_element_handler($this->parser, 'start_tag', 'end_tag');
        
        $this->parse();
        $this->flush();
    }


    function start_tag($parser, $node_name, $attributes) {
        if ($node_name === self::NODE_NAME) {
            $this->batch[] = $attributes;
        }
    }


    function end_tag($parser, $name) {
        if (count($this->batch) === self::BATCH_SIZE) {
            $this->flush();
        }
    }


    function parse() {
        $xml = is_readable($this->file) ? fopen($this->file, 'r') : false;

        if (!$xml) {
            trigger_error("Unable to open \"$this->file\"\n");
        }

        while (!feof($xml)) {
            $data = fread($xml, 4096);

            xml_parse($this->parser, $data, feof($xml));
        }
        
        fclose($xml);
    }
    
    
    function flush() {
        if ($this->batch) {
            $this->handler->process($this->batch);
        }

        unset($this->batch);
    }
}

?>