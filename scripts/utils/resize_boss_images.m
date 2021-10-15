% Function to resize the BOSS JPG images, as the originals are too large



% Read the CSV
filenames = readtable('C:\Users\lb08\Documents\jatos-3.3.6_win_java\jatos-3.3.6_win_java\study_assets_root\fast_schema_online\extra_files\choosing_images\high_familiarity_images.csv');

loadFolder = 'C:\Users\lb08\ownCloud\Cambridge\PhD\projects\fast_schema_mapping\pics\BOSS_Normative_v2(2014)_JPG';
saveFolder = 'C:\Users\lb08\Documents\jatos-3.3.6_win_java\jatos-3.3.6_win_java\study_assets_root\fast_schema_online\img\targets\BOSS\high_familiarity_downsized';

% Get all the files



% Reduction factor
N = 4;

for iFile = 1:height(filenames)
    
    iFile
    
    % read
    iImage = imread(fullfile(loadFolder,[filenames.FILENAME{iFile} '.jpg']));
        
    [rows, columns, numColorChannels] = size(iImage);
    numOutputRows = round(rows/N);
    numOutputColumns = round(columns/N);
    outputImage = imresize(iImage, [numOutputRows, numOutputColumns]);
   
    if iFile < 10
        prefix = '00';
    elseif iFile < 100
            prefix = '0';
    else
        prefix = '';
    end
    
    imwrite(outputImage,fullfile(saveFolder,[prefix int2str(iFile) '_' filenames.FILENAME{iFile} '.jpg']));
    
    
end