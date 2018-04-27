# Financial statement analysis and ST forecast.
 the data was from 狗熊会
    ST, 即special treatment, 由于被特别处理的企业面临着退市的风险，因此投资者需要对这类企业小心注意。所以，投资者有必要关心什么样的企业更有可能被ST？它们有什么共同特征？能否通过正常的财务报表分析有所察觉？本研究的目的是通过分析上市企业的公开财务报表信息，达到预测某企业未来两年内被ST的可能性，并以此警示投资风险。<br>
    数据`“财务报表分析与ST预测.csv”`来自狗熊会公众号<br>
    各指标的含义：ARA:应收账款与总资产的比例； ASSET:对数变换后的资产规模；ATO:资产周转率；ROA:资产收益率；GROWTH:销售收入增长率；LEV:债务资产比率；SHARE:企业第一大股东的持股比率。因变量Y为ST:是否被ST，0表示否，1表示是。<br>
    运行`ST_prediction.R`进行建模分析<br>
    本报告分析了上市企业的公开财务报表信息，建立了对企业未来ST状态具有一定预测能力的逻辑回归模型。我们的分析表明：1.企业的盈利质量（ARA）是影响企业未来ST可能性的最重要的因素，值得关注。2.企业的增长速度（GROWTH）和债务资产比率（LEV）也是会影响企业未来ST可能性的显著因素。
