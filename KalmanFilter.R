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
