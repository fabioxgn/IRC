program LightIRC;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
	cthreads,
	cmem, {$ENDIF} {$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Sysutils,
	Forms,
	Main,
	IRC,
	config,
	ConfigForm,
	TreeViewHelper,
	idircconfig,
	IRCViewIntf,
	quitcommand,
	command,
	partedcommand,
	joinedcommand;

{$R *.res}

begin
	{$IFDEF DEBUG}
	DeleteFile('heap.trc');
	SetHeapTraceOutput('heap.trc');
	{$ENDIF DEBUG}

	Application.Initialize;
	Application.CreateForm(TMainForm, MainForm);
	Application.Run;
end.
