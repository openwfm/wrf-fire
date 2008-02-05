n=read_model_test_out('model_test_out.txt',6);
r=read_model_test_out('model_test_out.txt.ref',6);
for i=1:6,max(max(abs((r.d(i).lfn-n.d(i).lfn)))),end