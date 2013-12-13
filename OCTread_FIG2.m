function [D,textItems] = OCTread_FIG2(fname,FLAG_PLOT)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   [D,items] = OCTread_FIG(fname,FLAG_PLOT)
%%
%%   OCTread_FIG is a simple function that extracts the plot data from
%%   a MATLAB .fig file. It does this by loading the file, which is
%%   in struct-format, then parsing down the structure to pull out the data
%%  
%%   D = a struct array containing the plot data, eg.
%%       plot(D(1).x,D(1).y) would plot the first set of data
%%
%%   textItems = a struct array of strings containing text in the figure.
%%               the axis labels come into here, and I just store them all.
%%
%%   fname = the name of the *.fig file (used for MATLAB R2011a
%%   FLAG_PLOT = set to 1 to plot the data (default is 0)
%%
%%
%%   Working in Octave 3.6.2, but when a figure contains a legend, Octave will
%%   pop-up a bunch of warnings as it tries to load MATLAB files.
%%
%%   modify as you like! 
%%   Cibby Pulikkaseril, 2013
%%   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist('FLAG_PLOT')
    FLAG_PLOT = 0;
end

aa = load(fname);                                                       %% load file into memory, and start climbing down the struct
aName = fieldnames(aa);
bb = getfield(aa,aName{1});

cc = bb.children;

for zz = 1:length(cc)

    dd = cc(zz).children;

    nItems = length(dd);
    kk = 1;                                                         %% counter for plots
    jj = 1;                                                         %% counter for text
    textItems = {};

    if FLAG_PLOT == 1
        figure(zz);
        hold on;
        col = repmat('rgmcym',1,round(nItems/6)+1);
    end

    for ii = 1:nItems

        ee = dd(ii);
        
        if strcmp(ee.type,'graph2d.lineseries')                         %% check if 2D data
            ff = ee.properties;
    
            D(zz,kk).x = ff.XData;
            D(zz,kk).y = ff.YData;


            if FLAG_PLOT == 1
                plot(D(zz,kk).x,D(zz,kk).y,col(kk));
            end
            kk = kk + 1;
        elseif strcmp(ee.type,'text')   
            ff = ee.properties;
            if isfield(ff,'String')
                textItems{jj} = ff.String;
                jj = jj + 1;
            end
        end
    end

end

for fig = 1:length(D)

    file = fopen(strcat('plot', mat2str(fig), '.csv'), 'w')
    fprintf(file, 'a,b,L\n')

    data = D(fig,:)
    for kk = 1:length(data)
        z = kk * 5 + 0.5
        x = data(kk).x
        y = data(kk).y
        for jj = 1:length(x)
            fprintf(file, '%f,%f,%f\n', x(jj), y(jj), z)
        end
    end

    fclose(file)

end
