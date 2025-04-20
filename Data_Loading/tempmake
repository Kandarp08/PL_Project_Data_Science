main: main.cmo dataframe.cmo row.cmo data_object.cmo utils.cmo datatypes.cmo
	ocamlc -o main utils.cmo datatypes.cmo data_object.cmo row.cmo dataframe.cmo main.cmo

main.cmo: main.ml dataframe.cmi row.cmi data_object.cmi utils.cmi datatypes.cmi
	ocamlc -c main.ml

dataframe.cmo: dataframe.ml dataframe.cmi row.cmi utils.cmi data_object.cmi datatypes.cmi
	ocamlc -c dataframe.ml

dataframe.cmi: dataframe.mli row.cmi datatypes.cmi data_object.cmi
	ocamlc -c dataframe.mli

row.cmo: row.ml row.cmi data_object.cmi datatypes.cmi
	ocamlc -c row.ml

row.cmi: row.mli data_object.cmi datatypes.cmi
	ocamlc -c row.mli

data_object.cmo: data_object.ml data_object.cmi datatypes.cmi utils.cmi
	ocamlc -c data_object.ml

data_object.cmi: data_object.mli datatypes.cmi
	ocamlc -c data_object.mli

datatypes.cmo: datatypes.cmi utils.cmi datatypes.ml
	ocamlc -c datatypes.ml

datatypes.cmi: datatypes.mli
	ocamlc -c datatypes.mli

utils.cmo: utils.ml utils.cmi
	ocamlc -c utils.ml

utils.cmi: utils.mli
	ocamlc -c utils.mli

clean:
	rm main *.cmi *.cmo