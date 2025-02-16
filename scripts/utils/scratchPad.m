%% Estimate learning rates
clear; clc;

warning('off','MATLAB:table:RowsAddedExistingVars')

%% Load and prepare the dataset

df = readtable('./results/pilots/preprocessed_data/mean_by_rep_all_types_long.csv');

% Get only the needed accuracy types
indices = strcmp(df.accuracy_type,'correct_exact') | strcmp(df.accuracy_type,'correct_one_square_away');
df = df(indices,:);

% Transform strings to doubles
df.correct_mean = str2double(df.correct_mean);
df.correct_sd = str2double(df.correct_sd);

all_ptp = unique(df.ptp_trunk);
n_ptp = length(all_ptp);

all_conditions        = unique(df.condition);
all_accuracy_types    = unique(df.accuracy_type);
all_new_pa_statuses = unique(df.new_pa_status);

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
            for iNeigh = 1:length(all_new_pa_statuses)
                iNeigh
                if strcmp(all_conditions{iCond},'no_schema') | strcmp(all_conditions{iCond},'random_locations')
                    
                    if strcmp(all_new_pa_statuses{iNeigh},'island') | strcmp(all_new_pa_statuses{iNeigh},'neighbor')
                    
                        continue;
                    
                    end
                    
                end
                
                curr_ptp   = all_ptp{iPtp};
                curr_cond  = all_conditions{iCond};
                curr_acc   = all_accuracy_types{iAccType};
                curr_neigh = all_new_pa_statuses{iNeigh};
                
                % Get the data
                y = df.correct_mean(strcmp(df.ptp_trunk,curr_ptp) &...
                    strcmp(df.condition,curr_cond) & ...
                    strcmp(df.new_pa_status,curr_neigh) &...
                    strcmp(df.accuracy_type,curr_acc));
                
%                 y = str2double(y);
                
                % Now fit the data
                [out_params,fval] = est_learning_rate(y',params,plotFMSEstimation);
                
                % Save in a table
                tbl.ptp_trunk{ctr} = curr_ptp;
                tbl.condition{ctr} = curr_cond;
                tbl.new_pa_status{ctr} = curr_neigh;
                tbl.accuracy_type{ctr} = curr_acc;
                tbl.sse(ctr) = fval;
                tbl.intercept(ctr) = out_params(1);
                tbl.learning_rate(ctr) = out_params(2);
                
                ctr = ctr + 1;
            end
        end
    end
end

%% Save the table
writetable(tbl,'./results/pilots/preprocessed_data/learning_rate_fits_matlab.csv');

