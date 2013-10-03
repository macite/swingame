import os

languages = [   
            {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas", "extension": ".pas"}, 
            {"lang": "C", "template": "../../Dist/CPP/gpp", "main file": "main.cpp", "extension": ".cpp"} 
        ]

script_path         = os.path.dirname(os.path.realpath(__file__)) + '/'
swingame_path       = os.path.realpath(script_path + '../..') + '/'
dist_directory      = os.path.join(swingame_path, 'Dist') + '/'
test_directory      = os.path.join(swingame_path, 'CoreSDK', 'test') + '/'
sg_wrapper_path     = os.path.join(swingame_path, 'Tools', 'SGWrapperGen', 'sg', 'pas_model') + '/'


def get_dist_directory():
	return dist_directory # "../../Dist/"

def get_test_directory():
	return test_directory  #"../../CoreSDK/test/"

def get_test_resource_directory():
	return get_test_directory() + 'Resources/'

def get_how_to_directory():
	return get_dist_directory() + "HowTo/"

def get_how_to_source_directory():
	return get_how_to_directory() + 'Source_Code/'

def get_parser_directory():
	return sg_wrapper_path #"../SGWrapperGen/sg/pas_model/"