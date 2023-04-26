# Import libraries.
from pathlib import Path
import pandas as pd


class DataPreprocessing:
    def __init__(self, data_path: Path):
        self.data_path = data_path

    def worksheet_reader(self) -> pd.DataFrame:
        """
        For parsing excel and csv dataset
        :return:
        """
        data = None
        if self.data_path.suffix == '.xlsx':
            data = pd.read_excel(self.data_path)
        elif self.data_path.suffix == '.csv':
            data = pd.read_csv(self.data_path)

        return data.head()


if __name__ == '__main__':

    eda_obj = DataPreprocessing(
        Path('/Dataset/Africa_Dataset/DHS_surveys_countries.csv')
    )
    print(eda_obj.worksheet_reader())
