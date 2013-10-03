import os

_current_dir = os.path.dirname(__file__)
_swingame_root = os.path.dirname(os.path.dirname(_current_dir))
_generated_dir = os.path.join(_swingame_root, 'Generated')

_generated_directories = {
	'C': 				['lib'],
	'CSharp': 			['Code','lib'],
	'Documentation': 	['html', 'sql'],
	'ObjC': 			['lib'],
	'Pascal': 			['lib'],
	'Source': 			['src']
}

def _make_dirs(base, subpaths):
	for name in subpaths:
		path = os.path.join(_generated_dir, base, name)
		if not os.path.exists(path):
			os.makedirs(path)

def create_generated_folders():
	for key, subpaths in _generated_directories.iteritems():
		_make_dirs(key, subpaths)

create_generated_folders()