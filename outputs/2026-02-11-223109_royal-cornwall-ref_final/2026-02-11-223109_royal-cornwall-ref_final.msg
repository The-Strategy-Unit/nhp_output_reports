Error in if (nchar(operation) > 0 && substr(operation, 1, 1) != "/")
paste0(container$name, : missing value where TRUE/FALSE needed
Traceback:
6: do_container_op(object, blob, options = opts, http_verb = "HEAD")
5: get_storage_properties.blob_container(container, src, snapshot = snapshot, 
       version = version)
4: get_storage_properties(container, src, snapshot = snapshot, version = version)
3: download_blob_internal(container, src, dest, blocksize = blocksize, 
       overwrite = overwrite, lease = lease, check_md5 = check_md5, 
       snapshot = snapshot, version = version)
2: AzureStor::download_blob(container, file, temp_file) at azure.R#105
1: get_nhp_results(file = secondary_file)
