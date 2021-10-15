clear; clc;

loadFolder = 'C:\Users\lb08\Documents\jatos-3.3.6_win_java\jatos-3.3.6_win_java\study_assets_root\fast_schema_online\img\targets\BOSS\high_familiarity_downsized\';

subfolder = 'practice2';


% Read the files
filenames = lsDir(fullfile(loadFolder,subfolder),{'jpg'})';
newfilenames = {};


for iFile = 1:length(filenames)
    
    cf = filenames{iFile};
    
    [a,b] = fileparts(cf);
    
    newFile = fullfile('img/targets/BOSS/high_familiarity_downsized',subfolder,[b '.jpg']);
    newfilenames{iFile,1} = newFile;
end
    