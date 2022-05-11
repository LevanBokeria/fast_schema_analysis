%% Description
 
% This script will load the pilot data, and for each participant and each condition,
% it will fit a learning curve to the data. The data being performance on 8 repetitions of PAs.
% 
% - If the performance variable is accuracy, it should increase over the 8 reps. 
% This is fit with an inverse exponential, estimating the intercept and learning rate.
% - If the performance variable is the mouse click euclidean error, that decreases over the 8 reps, 
% so its fit with an exponential decay function.

%% Estimate learning rates
clear; clc;
dbstop if error;

warning('off','MATLAB:table:RowsAddedExistingVars')

saveData = 1;

%% Load and prepare the dataset
opts = detectImportOptions('./results/pilots/preprocessed_data/mean_by_rep_all_types_long.csv');
opts = setvartype(opts,{'border_dist'},'char');


df = readtable('./results/pilots/preprocessed_data/mean_by_rep_all_types_long.csv',opts);

% Get only the needed accuracy types
indices = strcmp(df.border_dist,'all') | strcmp(df.border_dist,'3_4');
df = df(indices,:);

% Transform strings to doubles
% df.correct_mean = str2double(df.correct_mean);
% df.correct_sd    = str2double(df.correct_sd);
% df.correct_mean  = str2double(df.correct_mean);
df.accuracy_type = convertCharsToStrings(df.accuracy_type);

all_ptp = unique(df.ptp_trunk);
n_ptp   = length(all_ptp);

all_conditions        = unique(df.condition);
all_accuracy_types    = unique(df.accuracy_type);
all_new_pa_statuses   = unique(df.new_pa_status);
all_border_dist       = unique(df.border_dist);

%% Start the for loop
params = [0.5,0.1];
plotFMSEstimation = 0;

tbl = table;

ctr = 1;
for iPtp = 1:n_ptp
    iPtp
    for iCond = 1:length(all_conditions)
        iCond
        
        for iAccType = 1:length(all_accuracy_types)
            iAccType
            
            
            for iBordDist = 1:length(all_border_dist)
                iBordDist
                
                for iNeigh = 1:length(all_new_pa_statuses)
                    iNeigh
                    
                    if strcmp(all_conditions{iCond},'no_schema') | strcmp(all_conditions{iCond},'random_locations')
                        
                        if strcmp(all_new_pa_statuses{iNeigh},'near_pa') | strcmp(all_new_pa_statuses{iNeigh},'far_pa')
                            
                            continue;
                            
                        end
                        
                    end
                    
                    curr_ptp   = all_ptp{iPtp};
                    curr_cond  = all_conditions{iCond};
                    curr_acc   = all_accuracy_types{iAccType};
                    curr_neigh = all_new_pa_statuses{iNeigh};
                    curr_bord  = all_border_dist{iBordDist};
                    
                    % Get the data
                    y = df.correct_mean(strcmp(df.ptp_trunk,curr_ptp) &...
                        strcmp(df.condition,curr_cond) & ...
                        strcmp(df.new_pa_status,curr_neigh) &...
                        strcmp(df.accuracy_type,curr_acc) & ...
                        strcmp(df.border_dist,curr_bord));
                    
                    %                 y = str2double(y);
                    
                    % Now fit the data
                    [out_params,fval] = est_learning_rate(y',params,plotFMSEstimation,curr_acc);
                    
                    % Save in a table
                    tbl.ptp_trunk{ctr} = curr_ptp;
                    tbl.condition{ctr} = curr_cond;
                    tbl.new_pa_status{ctr} = curr_neigh;
                    tbl.accuracy_type{ctr} = curr_acc;
                    tbl.border_dist{ctr} = curr_bord;
                    tbl.sse(ctr) = fval;
                    tbl.intercept(ctr) = out_params(1);
                    tbl.learning_rate(ctr) = out_params(2);
                    
                    ctr = ctr + 1;
                end %iNeigh
            
            end %iBordDist
        end %iAccType
    end % iCond
end %iPtp

%% Save the table
if saveData
    writetable(tbl,'./results/pilots/preprocessed_data/learning_rate_fits_matlab.csv');
end
