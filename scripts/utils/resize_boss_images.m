% Function to resize the BOSS JPG images, as the originals are too large

loadFolder = 'C:\Users\lb08\ownCloud\Cambridge\PhD\projects\fast_schema_mapping\pics\BOSS_JPG';
saveFolder = 'C:\Users\lb08\ownCloud\Cambridge\PhD\projects\fast_schema_mapping\pics\BOSS_JPG_downsized';
% Get all the files
all_files = lsDir(loadFolder,{'jpg'})';

% Reduction factor
N = 4;

for iFile = 1:numel(all_files)
    
    iFile
    
    % read
    iImage = imread(all_files{iFile});
    
    [a,fileName] = fileparts(all_files{iFile});
    
    [rows, columns, numColorChannels] = size(iImage);
    numOutputRows = round(rows/N);
    numOutputColumns = round(columns/N);
    outputImage = imresize(iImage, [numOutputRows, numOutputColumns]);
    
    imwrite(outputImage,fullfile(saveFolder,[fileName '.jpg']));
    
    
end