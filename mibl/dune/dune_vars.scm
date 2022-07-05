
run vars:

(run %{deps} ...)

(run bash %{dep:rejections.sh} ...)

 (run %{exe:main.exe} -v -q)

 %{exe:test_requester.exe}

 %{lib:tezos-tooling:lint.sh}

;; ocp-ocamlres: "to embed files and directories inside OCaml executables"
(action
  (run %{bin:ocp-ocamlres} -format ocaml -o %{targets}
       %{lib:stdlib:camlinternalFormatBasics.cmi}
       %{dep:.tezos_protocol_registerer.objs/byte/tezos_protocol_registerer__Registerer.cmi}
       %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs.cmi}
       %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V0.cmi}
       %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V1.cmi}
       %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V2.cmi}))


 %{libexec:tezos-stdlib:test-ocp-indent.sh}

 %{ocaml}

%{targets}
%{workspace_root}

%{bin:tezos-embedded-protocol-packer}


%{tezos-protocol-compiler:lib}%
