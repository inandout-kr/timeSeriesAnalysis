## R

## 로켓은 시간 단계를 100번 거침. rocket will take 100 time steps
ts.length <- 100

## 가속도가 움직임을 주도. the acceleration will drive the motion
a <- rep(0.5, ts.length)

## 위치와 속도는 0에서 시작. position and velocity start at 0
x <- rep(0, ts.length)
v <- rep(0, ts.length)
for (ts in 2:ts.length) {
  x[ts] <- v[ts - 1] * 2 + x[ts - 1] + 1/2 * a[ts-1] ^ 2
  x[ts] <- x[ts] + rnorm(1, sd = 20) ## 확률적 요소. stochastic component
  v[ts] <- v[ts - 1] + 2 * a[ts-1]
}


par(mfrow = c(3, 1))
plot(x, main = "Position", type = 'l')
plot(v, main = "Velocity", type = 'l')
plot(a, main = "Acceleration", type = 'l')

z <- x + rnorm(ts.length, sd = 300)
plot (x, ylim = range(c(x, z)))
lines(z)


## Kalman Filter
kalman.motion <- function(z, Q, R, A, H) {
  dimState = dim(Q)[1]
  
  xhatminus <- array(rep(0, ts.length * dimState),
                     c(ts.length, dimState)) # x hat-
  xhat <- array(rep(0, ts.length * dimState),
                c(ts.length, dimState)) # x hat
  Pminus <- array(rep(0, ts.length * dimState * dimState),
                  c(ts.length, dimState, dimState)) # P-
  P <- array(rep(0, ts.length * dimState * dimState),
             c(ts.length, dimState, dimState)) # P
  
  K <- array(rep(0, ts.length * dimState),
             c(ts.length, dimState)) # K, Kalman gain
  
  # 초기 추측 = 모든 지표는 0으로 시작함. intial guesses = starting at 0 for all metrics
  xhat[1, ] <- rep(0, dimState)
  P[1, , ] <- diag(dimState)
  
  for (k in 2:ts.length) {
    # Prediction
    xhatminus[k, ] <- A %*% matrix(xhat[k-1, ]) # %*%: 행렬 곱셈 연산자
    Pminus[k, , ] <- A %*% P[k-1, , ] %*% t(A) + Q
    
    # Filtering
    K[k, ] <- Pminus[k, , ] %*% H %*%
      solve( t(H) %*% Pminus[k, , ] %*% H + R )
    xhat[k, ] <- xhatminus[k, ] + K[k, ] %*%
      (z[k]- t(H) %*% xhatminus[k, ])
    P[k, , ] <- (diag(dimState)-K[k,] %*% t(H)) %*% Pminus[k, , ]
  }
  
  ## 예측과 평활화된 값 모두 반환. we return both the forecast and the smoothed value
  return(list(xhat = xhat, xhatminus = xhatminus))
}



## noise parameters
R <- 10^2 ## 측정 분산 - 이 값은 측정 도구에 대해 알려진, 
## 물리적인 한계에 따라 설정되어야 한다.
## x에 더해진 노이즈와 일관성 있게 설정한다.

Q <- 10 ## 과정의 분산 - 일반적으로 성능의 최대화를 위해서
## 조정되어야 하는 하이퍼파라미터로 취급된다.

A <- matrix(1) ## x_t = A * x_(t-1) (이전 x가 나중의 x에 얼마나 영향을 미치는지. how prior x affects later x)
H <- matrix(1) ## y_t = H * x_t (상태를 측정으로 변환. translating state to measurement)

## 칼만 필터 method로 데이터를 넣고 돌림. run the data through the Kalman filtering method
xhat <- kalman.motion(z, diag(1) * Q, R, A, H)[[1]]

