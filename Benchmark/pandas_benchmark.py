import pandas as pd
import numpy as np
import time
import tracemalloc

def benchmark(name, func, *args, **kwargs):
    tracemalloc.start()
    start_time = time.time()
    result = func(*args, **kwargs)
    elapsed_time = time.time() - start_time
    current, peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()
    print(f"{name:<20} | Time: {elapsed_time:.6f}s | Memory: {peak / 1024:.2f} KB")
    return result

def load_csv(file_path):
    return pd.read_csv(file_path)

def map_column(df, column, func):
    df[column] = df[column].map(func)

def filter_column(df, column, func):
    df[df[column].map(func)]

def fold_column(df, column, func, initial):
    df[column].agg(lambda col: np.fromiter((func(initial, x) for x in col), dtype='object')[-1])

def normalize_column(df, column):
    df[column] = (df[column] - df[column].mean()) / df[column].std()

def min_max_normalize(df, column):
    min_val = df[column].min()
    max_val = df[column].max()
    df[column] = (df[column] - min_val) / (max_val - min_val)

def imputena(df, column):
    df[column] = df[column].fillna(df[column].mean())

def fillna(df, column, value):
    df[column] = df[column].fillna(value)


def join(df1, df2, on_column):
    pd.merge(df1, df2, on=on_column)

def group_by_aggregate(df, group_col, agg_spec):
    df.groupby(group_col).agg(agg_spec).reset_index()

def iloc (df, i, j):
    df.iloc[i:(j+1)]

def create_dataset(rows=100000):
    df =  pd.DataFrame({
        "id": range(rows),
        "value": np.random.rand(rows) * 100,
        "category": np.random.choice(["A", "B", "C"], rows),
        "nullable": np.where(np.random.rand(rows) > 0.8, np.nan, np.random.rand(rows) * 50)
    })

    df.to_csv("test1.csv")

    return df

# --- Run Benchmarks ---
if __name__ == "__main__":
    df = create_dataset()

    benchmark("Load CSV", load_csv, "test1.csv")
    benchmark("Map", map_column, df, "value", lambda x: x * 2)
    benchmark("Filter", filter_column, df, "value", lambda x: x > 50)
    benchmark("Fold", fold_column, df, "value", lambda acc, x: acc + x, 0)
    benchmark("Normalize", normalize_column, df, "value")
    benchmark("MinMax Normalize", min_max_normalize, df, "value")
    benchmark("Impute NA", imputena, df, "nullable")
    benchmark("Fill NA", fillna, df, "nullable", 0)
    benchmark("iLoc", iloc, df, 1345, 5431)
    benchmark("GroupBy-Agg", group_by_aggregate, df, "category", {"value": "mean", "nullable": "sum"})
