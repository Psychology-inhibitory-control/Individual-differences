% --------------------------------- Initialize ---------------------------------
%%% Clear the workspace and the screen
sca;
close all;
clear all;

%%% PsychToolBox settings
Screen('Preference', 'SuppressAllWarnings', 1);            % Remove all warnings
Screen('Preference', 'SkipSyncTests', 1);                  % Skip sync tests

%%% Gather info about available displays
screens = Screen('Screens');   % Get the screen numbers
screenNumber = max(screens);   % Draw to the (last) external screen if avaliable

%%% Other settings (keyboard, default colors, etc)
KbName('UnifyKeyNames');
escKey = KbName('ESCAPE');
RestrictKeysForKbCheck(escKey);

white = [255, 255, 255];
black = [0, 0, 0];
gray = [127, 127, 127];
red = [255, 0, 0];
k=0;


%%% Seed the random numbers generator
%rand('state', sum(100*clock));        % Use this with GNU Octave but NOT Matlab
rng('default'); rng('shuffle');        % Use this with Matlab


interTrialInterval = 2;  % Time between two trials, in seconds
respLimit = 15;            % Time limit for response: passed this, program stops
maxTrials = 36;


%%% Sound feedback
BeepFreq = [800, 1300, 2000];
BeepDur = [0.1, 0.1, 0.1];
Beep1 = MakeBeep(BeepFreq(1), BeepDur(1));
Beep2 = MakeBeep(BeepFreq(2), BeepDur(2));
Beep3 = MakeBeep(BeepFreq(3), BeepDur(3));
Beep4 = [Beep1, Beep2, Beep3];


% ------------------------- Prompt the experimenter ----------------------------

%%% Login prompt and open file for writing data out
prompt = { 'Subject ID','Output file', 'Gender', 'Session'};
defaults = {'','ChoiceRT', 'M', '1'};

answer = inputdlg(prompt, 'ChoiceRT', 2, defaults);  % Get experimenter's answers
[s_subjID, s_output,  s_gender, s_session] = deal(answer{:});
% All answers are strings, hence the 's_'

outputname = [s_subjID, s_output, s_gender,  s_session, '.csv'];  % Generate output file name

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
fprintf(outfile, '%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\n', 'subj_id', 'gender', 'session', 'trial', 'react_time', 'nb_taps', 'stim_pos', 'picture','att');

% ------------------------------- Display stuff --------------------------------

[window, screenRect] = Screen('OpenWindow', screenNumber);
%HideCursor();

%%% coordinates of the display
[screenWidth, screenHeight] = Screen('WindowSize', window);     % Get screen size
[xCenter, yCenter] = RectCenter(screenRect);   % Coordinates of the screen center

Screen('FillRect', window, white);        % Fill the window with white
Screen(window, 'Flip');                   % Refresh the screen to display changes

%%% Load the image
im = imread('redCross.jpg');                   % Open the .jpg file
redCross = Screen('MakeTexture', window, im);  % Generate the texture to display

rect = [0, 0, 360, 500];     % Make 100x100 rectangle (at the origin 0,0)taille

% And get coordinates for its two posible
% positions======================================
rectCoordsLeft = CenterRectOnPointd(rect, screenWidth*0.15 - 50, yCenter-250);
rectCoordsRight = CenterRectOnPointd(rect, screenWidth*0.85 + 50, yCenter-250);


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

if Continue == false
 % picsVector = zeros(maxTrials,1);
 picsVector = zeros(100,1);  
  OrderBloc = Shuffle([1,1,2,2,3,3]);
  BlocNum =1;
  for i = 3:6:maxTrials
      OrderPic = Shuffle([1,2,3,4]);
      picsVector(i:(i+3)) = (OrderBloc(BlocNum) -1)*4 + OrderPic;
      BlocNum = BlocNum+1;
  end
end
  
iteration = 1:maxTrials;
if Continue
    i = i+1;
    iteration = i:maxTrials;
end
%%% Loop for each trial
for i = iteration              % i will increment in a normal (non-random) way

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
  
  % If we use scary pictures:
  % Every N trials, the modulo of i by N is equal to 0,
  % so that displays the scary pic every N trials 
 
currPic = picsVector(i);
if currPic > 0 && currPic < 5
    pictureName = ['object', num2str(currPic), '.jpg'];
    picIm = imread(pictureName);
    picTex = Screen('MakeTexture', window, picIm);
    Screen('DrawTexture', window, picTex,[],[290,0,990,600]);
    
elseif currPic > 4 && currPic < 9
    pictureName = ['threat', num2str(currPic -4), '.jpg'];
    picIm = imread(pictureName);
    picTex = Screen('MakeTexture', window, picIm);
    Screen('DrawTexture', window, picTex,[],[290,0,990,600]);
   
elseif currPic > 8 
    pictureName = ['neutral', num2str(currPic -8), '.jpg'];
    picIm = imread(pictureName);
    picTex = Screen('MakeTexture', window, picIm);
    Screen('DrawTexture', window, picTex,[],[290,0,990,600]);
   
end


  % Look if currTrial is even or odd with modulo 2
  if mod(currTrial, 2)==0        
    rectPos = rectCoordsLeft;      % If even, use Left coords for stimulus
    s_stimPos = 'left';
  else
    rectPos = rectCoordsRight;     % If odd, use Right coords
    s_stimPos = 'right'; 
  end

  Screen('FillRect', window, red, rectPos);     % Display target
  Screen('Flip', window);
  
  %%% Record response
  timeStart = GetSecs;                          % Get time

  % while the monkey has not touched the target
  while correct==0
     k=k+1;
    % Make sure the mouse button / touchscreen is not in use
    while any(touch)
      [x, y, touch] = GetMouse(window, []);
    end
    
    % while the screen / mouse button is not pressed, wait for next press
    while ~any(touch)
      
      % check if experimenter pressed Escape...
      [ keyIsDown, keyTime, keyCode ] = KbCheck;
      
      if keyIsDown
        sca;
        fclose(outfile);
        return;
      end
      % ...if so or if monkey abandoned (time > limit), properly stop everything
      if GetSecs - timeStart > respLimit
       Screen('DrawTexture', window, redCross);
       Screen('Flip', window);
       GetClicks([], 0, []);
       att=0;
       correct=1;
      end
      
      % Detect screen touch / mouse button press...
      [xClick, yClick, touch] = GetMouse(window, []);
      end
    % ...and wait for its release
    while any(touch)
      [x, y, touch] = GetMouse(window, []);
    end
    taps = taps + 1;  % Increment the touch counter
    
    % if the current detected touch is in the target, we're good to go
     if rectPos(1)-2000 < xClick && xClick < rectPos(3)+2000 && rectPos(2)-2000 < yClick && yClick < rectPos(4)+2000 
      correct = 1;  % Set correctness to 1 to exit the while loop
      
    else
   
    end
  end

  timeResponse = GetSecs;                        % Get time again
  
  % Response time is the difference between the two times
  rt = (timeResponse - timeStart) * 1000;
  
  % Make a beep
  Snd('Play', Beep4);

  % Empty screen
  Screen('FillRect', window, white);
  Screen('Flip', window);

  %%% Write data out
  fprintf(outfile, '%10s\t%10s\t%10s\t%10d\t%10.4f\t%10d\t%10s\t%10d\t%10d\n', s_subjID, s_gender, s_session, i, rt, taps, s_stimPos, currPic, att);
  save( ['DATA', s_subjID, '.mat'] , 'picsVector', 'i');
  % Wait for a while before starting next trial
  WaitSecs(interTrialInterval);
  

end
 

% If the maxTrials number is reached, properly close the display
% and save the output file
sca;
fclose(outfile);
