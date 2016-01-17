object dmMain: TdmMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 245
  Width = 360
  object SearchDir: TSearchDir
    OnFindFile = SearchDirFindFile
    Left = 16
    Top = 16
  end
end
