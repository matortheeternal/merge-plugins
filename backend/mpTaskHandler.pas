unit mpTaskHandler;

interface

uses
  Classes, SysUtils,
  // mte components
  mpBackend, mteLogger, mteHelpers;

type
  TProcedure = procedure of object;
  TTask = class (TObject)
  private
    FExecute : TProcedure;
  public
    name: string;
    rate: real;
    lastExecuted: TDateTime;
    constructor Create(name: string; rate: real; FExecute: TProcedure); Overload;
    property OnExecute: TProcedure read FExecute write FExecute;
    procedure Execute;
  end;
  TTaskHandler = class(TObject)
  public
    TaskList: TList;
    procedure AddTask(task: TTask);
    procedure ExecTasks;
    constructor Create; Overload;
  end;

implementation

procedure TTaskHandler.AddTask(task: TTask);
begin
  TaskList.Add(task);
end;

procedure TTaskHandler.ExecTasks;
var
  i: Integer;
  task: TTask;
begin
  // loop through task list, executing tasks that are ready to be executed
  for i := 0 to Pred(TaskList.Count) do begin
    task := TTask(TaskList[i]);
    if (Now - task.lastExecuted >= task.rate) then begin
      if (task.rate > 60.0 * seconds) then
        Logger.Write('TASK', 'Execute', task.name);
      task.Execute;
      task.lastExecuted := Now;
    end;
  end;
end;

constructor TTaskHandler.Create;
begin
  TaskList := TList.Create;
end;


{******************************************************************************}
{ Task methods
  Object methods for TTask
}
{******************************************************************************}

constructor TTask.Create(name: string; rate: real; FExecute: TProcedure);
begin
  Logger.Write('TASK', 'Init', Format('%s, Rate: %s', [name, RateStr(rate)]));
  self.name := name;
  self.rate := rate;
  self.FExecute := FExecute;
  self.lastExecuted := Now;
end;

procedure TTask.Execute;
begin
 if Assigned(FExecute) then
   FExecute;
end;

end.

