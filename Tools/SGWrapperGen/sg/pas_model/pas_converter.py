def run_convert(file):
    """
    This method converts a file
        
    """
    if file.contains_kind == 'program':
        file.to_code()
    return file