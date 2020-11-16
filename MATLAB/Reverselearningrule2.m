

% --------------------------------- Initialize ---------------------------------

%% Clear the workspace and the screen
sca;
close all;
clear all;

%%% PsychToolBox settings
Screen('Preference', 'SuppressAllWarnings', 1);            % Remove all warnings
Screen('Preference', 'SkipSyncTests', 1);                  % Skip sync tests

%%% Gather info about available displays      )
KbName('UnifyKeyNames');
escKey = KbName('ESCAPE');
RestrictKeysForKbCheck(escKey);

%Set up coloursBeep1
white = [255, 255, 255];
red = [255, 0, 0];
green = [150, 255,150]; %intensite


screens = Screen('Screens');   % Get the screen numbers
screenNumber = max(screens);   % Draw to the (last) external screen if avaliable

%%% Other settings (keyboard, default colors, etcgreen = [0, 255,0]; 

%%% Seed the random numbers generator
%rand('state', sum(100*clock));        % Use this with GNU Octave but NOT Matlab
rng('default'); rng('shuffle');        % Use this with Matlab


interTrialInterval = 2;  % Time between two trials, in seconds
respLimit = 35;            % Time limit for response: passed this, program stops
maxTrials = 40;


%%% Sound feedback
BeepFreq = [800, 1300, 2000];
BeepDur = [0.1, 0.1, 0.1];
Beep1 = MakeBeep(BeepFreq(1), BeepDur(1));
Beep2 = MakeBeep(BeepFreq(2), BeepDur(2));
Beep3 = MakeBeep(BeepFreq(3), BeepDur(3));
Beep4 = [Beep1, Beep2, Beep3]; % Correct
Beep5 = [Beep3, Beep2, Beep1];%incorrect

% ------------------------- Prompt the experimenter ----------------------------

%%% Login prompt and open file for writing data out
prompt = {'Subject ID', 'Output file', 'Gender', 'Session'};
defaults = {'','Reverseleaning2',  'M', '1'};

answer = inputdlg(prompt, 'Reverselearning2', 2, defaults);  % Get experimenter's answers
[ s_subjID, s_output,s_gender, s_session] = deal(answer{:});% All answers are strings, hence the 's_'

outputname = [s_output, s_gender, s_subjID, s_session, '.csv'];  % Generate output file name

%%% Check if file exists
Continue = false;
if exist(outputname)==2                   % If file exists, ask what to do (3 possible answers)
    fileproblem = questdlg('This file already exists!', 'Existing file', 'Continue it', 'Overwrite it', 'Abort', 'Abort');
    if strcmp(fileproblem, 'Continue it') == 1         % If answer 1, open with 'append' ('a') mode
        outfile = fopen(outputname, 'a');
        load(['DATA', s_subjID, '.mat']);
        Continue = true;
    elseif strcmp(fileproblem, 'Overwrite it') == 1    % If answer 2, 'write' ('w') mode to overwrite
        outfile = fopen(outputname, 'w');
    else
        return;
    end
else              % If file does not exist, opening in write mode will create it
    outfile = fopen(outputname, 'w');
end

% Print the first line (columns headers)
fprintf(outfile, '%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\n', 'subj_id', 'gender', 'session', 'trial', 'react_time', 'nb_taps', 'stim_pos', 'goodresp','att','tapsov');


% ------------------------------- Display stuff --------------------------------
[window, screenRect] = Screen('OpenWindow', screenNumber);


%%% coordinates of the display
[screenWidth, screenHeight] = Screen('WindowSize', window);     % Get screen size
[xCenter, yCenter] = RectCenter(screenRect);   % Coordinates of the screen center

Screen('FillRect', window, white);        % Fill the window with white
Screen(window, 'Flip');                   % Refresh the screen to display changes

%%% Load the image
im = imread('redCross.jpg');                   % Open the .jpg file
redCross = Screen('MakeTexture', window, im);  % Generate the texture to display

rect = [0, 0, 480, 480];     % Make 100x100 rectangle (at the origin 0,0)taille

% And get coordinates for its two posible positions
rectCoordsLeft = CenterRectOnPointd(rect, screenWidth*0.25-50, yCenter-200);
rectCoordsRight = CenterRectOnPointd(rect, screenWidth*0.75+50, yCenter-200);

rectCoordsLeftOval = CenterRectOnPointd(rect, screenWidth*0.25+700, yCenter-200);
rectCoordsRightOval = CenterRectOnPointd(rect, screenWidth*0.75-700, yCenter-200);
% -------------------------------- Do the thing --------------------------------

% Display neutral stimulus (a cross)
Screen('FillRect', window, white);
Screen('DrawTexture', window, redCross);
Screen('Flip', window);

GetClicks([], 0, []);  % And wait until a click happens anywhere

Screen('FillRect', window, white);
Screen('Flip', window);

%%% Pre-generate the randomized elements :
trialsVector = Shuffle(1:maxTrials);             % Randomized vector
   % conditions = [zeros(1,numbergo),ones(1,maxTrials-numbergo)];
   rng('shuffle');
    

%%% Loop for each trial
iteration = 1:maxTrials;
if Continue
    i = i+1;
    iteration = i:maxTrials;
end
%%% Loop for each trial
for i = iteration
  KbReleaseWait();      % Make sure all keyboard keys are released
  touch = 1;            % Make sure the mouse button / touchscreen is not in use
  while any(touch)
    [x, y, touch] = GetMouse(window, []);
  end

  correct = 0;                       % Reset correctness
  rt = 0;                            % Reset reaction time
  taps = 0;                          % Reset number of taps
  att = 1;
  currTrial = trialsVector(i);       % currTrial is taken from the random vector
  goodresp = 0;
  tapsov = 0;

  % Look if currTrial is even or odd with modulo 2
  if mod(currTrial, 2)==0
      rectPos = rectCoordsLeft;      % If even, use Left coords for stimulus
      ovalPos = rectCoordsLeftOval;
      
      s_stimPos = 'left';
          
       Screen('FillOval', window, red, rectPos);
       Screen('FillRect', window, green, ovalPos);
  else
      rectPos = rectCoordsRight;     % If odd, use Right coords
      ovalPos = rectCoordsRightOval;
      
      s_stimPos = 'right';
        
      Screen('FillOval', window, red, rectPos);
      Screen('FillRect', window, green, ovalPos); 
  end
      
      
       Screen('Flip', window);
  
  %%% Record response
  timeStart = GetSecs;                          % Get time

  % while the monkey has not touched the target
  while correct==0
    
    % Make sure the mouse button / touchscreen is not in use
    while any(touch)
      [x, y, touch] = GetMouse(window, []);
    end
    
    %while the screen / mouse button is not pressed, wait for next press
    while ~any(touch) && correct == 0
      
      % check if experimenter pressed Escape...
      [ keyIsDown, keyTime, keyCode ] = KbCheck;
      
 % ...if so or if monkey abandoned (time > limit), fixation cross
      if keyIsDown
        sca;
        fclose(outfile);
        return;
      end
      
    if GetSecs - timeStart > respLimit
       Screen('DrawTexture', window, redCross);
       Screen('Flip', window);
       GetClicks([], 0, []);
       att = 0;
       correct=1;
    end
      
     
      % Detect screen touch / mouse button press...
      [xClick, yClick, touch] = GetMouse(window, []);
    end
   if correct ==0
    % ...and wait for its release
    while any(touch)
      [x, y, touch] = GetMouse(window, []);
    end
    taps = taps + 1;  % Increment the touch counter
   
        % if the current detected touch is in the go target, we're good to go
        if rectPos(1)-80 < xClick && xClick < rectPos(3)+80 && rectPos(2)-80 < yClick && yClick < rectPos(4)+80 && taps>=1 %go trials
                Snd('Play', Beep4);% Make the beep correct
                correct = 1;  % Set correctness to 1 to exit the while loop
                goodresp = 1;
                
        elseif ovalPos(1)-80 < xClick && xClick < ovalPos(3)+80 && ovalPos(2)-80 < yClick && yClick < ovalPos(4)+80&& taps>=1  % if click in the nogo target
                   Snd('Play', Beep5); % sound not correc10
                     goodresp = 0;
                     correct  = 0;
                     tapsov = tapsov+1;  
%                     Screen('FillRect', window, white);
%                     Screen('Flip', window);
%                     WaitSecs(3); %white screen for 1 sec attente
%                     correct = 1; %to exit the loop            
      
        end
   end
  end
  timeResponse = GetSecs;                        % Get time again
  
  % Response time is the difference between the two times
  rt = (timeResponse - timeStart) * 1000;
  
  % Empty screen
  Screen('FillRect', window, white);
  Screen('Flip', window);

  %%% Write data out
 fprintf(outfile, '%10s\t%10s\t%10s\t%10d\t%10.4f\t%10d\t%10s\t%10d\t%10d\t%10d\n', s_subjID, s_gender, s_session, i, rt, taps, s_stimPos, goodresp, att,tapsov);
   
  save( ['DATA', s_subjID, '.mat'] , 'trialsVector', 'i')
  % Wait for a while before starting next trial
  WaitSecs(interTrialInterval);

end

% If the maxTrials number is reached, properly close the display
% and save the output file
sca;
fclose(outfile);
