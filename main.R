#������ ��������� ������ �������, ����� ������

library("ggplot2")
library("httr")

response <- GET(
"https://apidata.mos.ru/v1/datasets/2253/rows?api_key=d709b0f704b1c986bb4207a6bf2ebf4b"
)
rubbish <- content(response)

rubbishdf <- data.frame(matrix(unlist(rubbish), ncol = 27, byrow = T))
nrubbishdf <- rubbishdf [, -c(4, 6, 7, 11, 12, 13, 14, 15, 17, 18, 19,
                             20, 21, 22, 23, 24, 25, 26, 27)]

colnames(nrubbishdf) <- c(
  "global_id", "number", "global_id1", "District",
  "Actual_inhabitants_number", "Municipal_solid_waste_volume",
  "Bulky_waste_volume", "Placed_containers"
)

moscow_district <- c("���", "���", "����", "���", "����", "����",
                     "����", "����", "����")
nrubbishdf <- cbind(nrubbishdf, moscow_district)

nrubbishdf[, "new1"] <- as.numeric(as.character(nrubbishdf[, "Placed_containers"]))
nrubbishdf[, "new2"] <- as.numeric(as.character(nrubbishdf[, "Actual_inhabitants_number"]))

#1
axes <- ggplot(data = nrubbishdf) +
  geom_bar(mapping = aes(x = moscow_district, fill = moscow_district, y = new1),
           stat = "identity", show.legend = FALSE)


axes + labs(title = "����������� ���������� (��.) �� ���������������� ������� � �. ������",
       subtitle = "��������: ������ �������� ������ ������������� ������") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(panel.background = element_rect(fill = "Black"),
        panel.grid.major.x = element_line(colour = "Black")) +
  scale_y_continuous(name = "���������� �����������") +
  scale_x_discrete(name = "���������������� �����") +
  geom_label(data = nrubbishdf, aes(x = moscow_district,
                                 y = new1,
                                 label = new1), vjust = 1.5)

#2

nrubbishdf <- transform(nrubbishdf, per1000 =
                          nrubbishdf$new1 /
                          nrubbishdf$new2 * 1000)
nrubbishdf <- transform(nrubbishdf, Nper1000 = round(nrubbishdf$per1000, 2))

axes1 <- ggplot(data = nrubbishdf) +
  geom_bar(mapping = aes(x = moscow_district, fill = moscow_district, y = Nper1000),
           stat = "identity", show.legend = FALSE)

axes + labs(title = "��������� ���������� ����������� (��.) �� ���. ������� � �. ������",
       subtitle = "��������: ������ �������� ������ ������������� ������") +
  scale_fill_brewer(palette = "Blues") +
  theme_classic() +
  scale_y_continuous(name = "���-�� ����������� �� 1000 ���. (��.)") +
  scale_x_discrete(name = "���������������� �����") +
  geom_label(data = nrubbishdf, aes(x = moscow_district,
                                 y = Nper1000,
                                 label = Nper1000), vjust = 2)

#3

axes3 <- ggplot(data = nrubbishdf) +
  geom_col(mapping = aes(x = moscow_district, fill = moscow_district, y = Nper1000))

axes3 + coord_polar() +
  labs(title = "��������� ���������� ����������� (��.) �� ���. ������� � �. ������",
       subtitle = "��������: ������ �������� ������ ������������� ������") +
  scale_fill_brewer(palette = "Purples") +
  theme(panel.background = element_rect(fill = "gray80")) +
  scale_y_continuous(name = "���-�� ����������� �� 1000 ���. (��.)") +
  scale_x_discrete(name = "���������������� �����") +
  geom_label(data = nrubbishdf, aes(x = moscow_district,
                                 y = Nper1000,
                                 label = Nper1000), vjust = 1)


library("lintr")
lint("main.R")
