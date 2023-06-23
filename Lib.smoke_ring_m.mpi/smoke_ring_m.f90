module smoke_ring_m
  use constants_m  !! 定数
  use field_m      !! スカラー場・ベクトル場
  use fluid_m      !! 流体場の構造体定義
  use grid_m       !! 格子点情報
  use job_m        !! ジョブ管理
  use kutimer_m    !! 時間測定モジュール ;call kutimer__('main  ','sample')
  use mpiut_m      !! mpi関係ユーティリティ
  use parallel_m   !! MPI並列化
  use params_m     !! パラメータ
  use solver_m     !! ナビエ・ストークス方程式ソルバ
  use ut_m         !! ユーティリティ
  use vis2d_m      !! 断面可視化
end module smoke_ring_m
