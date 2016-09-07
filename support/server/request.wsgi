import os.path
import imp
from cgi import parse_qs, escape

responseHeader = [('Content-Type', 'text/plain')]

baseUrl = %REQUIRED% URL TO THE BASE DIRECTORY OF FILES
baseFile = %REQUIRED% LOCAL PATH TO THE BASE DIRECTORY OF FILES
baseRequest = %REQUIRED% LOCAL PATH TO THE DIRECTORY OF REQUESTS

def find_file(identifier, file_version, application_version):
    file = identifier + "_v" + application_version + "_v" + file_version

    if os.path.isfile(baseFile + file):
        # If the direct file exists, it lists all necessary files line by line
        with open(baseFile + file, "r") as f:
            return True, f.read()
    elif os.path.isfile(baseRequest + file + '.py'):
        # If the direct file doesn't exist, we look for a python script that
        # will generate the returns string dynamically
        module = imp.load_source('files', baseRequest + file + '.py')
        return True, module.files()
    else:
        # If neither exists, an unknown file was requested
        return False, "Could not find identifier '" + file + "'"

def application(environ, start_response):
    parameters = parse_qs(environ.get('QUERY_STRING', ''))

    if 'identifier' in parameters:
        file = escape(parameters['identifier'][0])
    else:
        start_response('400 Bad Request', responseHeader)
        return ['No identifier provided']

    if 'file_version' in parameters:
        file_version = escape(parameters['file_version'][0])
    else:
        file_version = "1"

    if 'application_version' in parameters:
        application_version = escape(parameters['application_version'][0])
    else:
        application_version = "1"

    status_ok, url = find_file(file, file_version, application_version)

    if (status_ok):
        start_response('200 OK', responseHeader)
    else:
        start_response('400 Bad Request', responseHeader)

    return [url]