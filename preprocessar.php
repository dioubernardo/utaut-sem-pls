<?php
$row = 1;

$fp = fopen('pls.csv', 'w');

$outros = [];

$handle = fopen('dados.csv', 'r');
while (($data = fgetcsv($handle, null, ',')) !== FALSE) {

    $line = [];

    if ($row == 1) {
        $line = [
            'Genero',
            'Idade',
            'Experiencia',
            'ED1',
			'ED2',
			'ED3',
			'ED4',
			'EE1',
			'EE2',
			'EE3',
			'EE4',
			'IS1',
			'IS2',
			'IS3',
			'CF1',
			'CF2',
			'CF3',
			'CF4',
			'MH1',
			'MH2',
			'MH3',
			'PR1',
			'PR2',
			'PR3',
			'HA1',
			'HA2',
			'HA3',
			'HA4',
			'IC1',
			'IC2',
			'IC3',
            'USO1',
            'USO2',
            'USO3',
            'USO'
        ];
    } else {

        $line[0] = ($data[2] == 'Feminino' ? 0 : 1); // Dicotómica onde 0=Feminino é Referência
        $line[1] = empty($data[1]) ? 999 : $data[1];
        $line[2] = round($data[3] * 12);
		
		$nrovazios = 0;
		for($i=7;$i<=34;$i++){
			if (empty($data[$i])){
				$nrovazios++;
				$line[] = 999;
			}else{
				$line[] = $data[$i]; 
			}
		}

		if ($nrovazios > 4){
			echo "Removido carimbo '{$data[0]}' pois continha {$nrovazios} vazios\n";
			continue;
		}
		
		/* programação */
        $linguagens = ['Rstudio', 'Python', 'Java', 'C++', 'Linguagem R', 'R Commander'];
        $line[31] = consolida([$data[35] /* R */, $data[42] /* Python */, localiza($data, $linguagens)]);

		/* softwares estatisticos */
        $estatisticos = ['geogebra', 'SAS', 'Stata', 'Gretl', 'Statistica', 'Bioest', 'BIOSTAT', 'Past', 'Power Bi', 'JASP', 'Jamovi', 'Eviews', 'Systat', 'Sisvar', 'QGIS', 'Epiinfo','LabPlot2'];
        $line[32] = consolida([$data[39], $data[41], $data[43], $data[44], $data[36], $data[37], $data[40], localiza($data, $estatisticos)]);
 
		/* planilhas */
        $planilhas = ['Google sheet', 'Google Planilha', 'CALC', 'Excel'];
        $line[33] = consolida([$data[38], localiza($data, $planilhas)]);
		
		/* USO */
		$line[34] = max([$line[31], $line[32], $line[33]]);
    }

    fputcsv($fp, $line);

    $row++;
}

fclose($handle);
fclose($fp);

function consolida($v){
	$v = max($v);
	if ($v == 0)
		return 1;
	return $v;
}
function localiza($data, $itens)
{
    $reg = '/(' . implode('|', $itens) . ')/i';
    $ret = [0];
    if (preg_match($reg, $data[46]))
        $ret[] = $data[45];
    if (preg_match($reg, $data[48]))
        $ret[] = $data[47];
    if (preg_match($reg, $data[50]))
        $ret[] = $data[49];
    return max($ret);
}

